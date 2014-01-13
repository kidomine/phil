{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import System.Environment
import System.Directory
import System.Process
import System.IO
import Data.List (init, foldl', concat)
import Database.MongoDB 
import System.Exit (exitSuccess)
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Control.Concurrent
import Control.Exception
import Data.Int
import Data.Text (unpack)

import Utils
import Add
import Delete
import Get
import Migrate
import Review
import Validate
import Goals
import Done
import Edit

main :: IO ()
main = bracketOnError (initializeInput defaultSettings)
         cancelInput -- This will only be called if an exception such
                         -- as a SigINT is received.
         (\inputState -> loop inputState >> closeInput inputState)
  where
    loop :: InputState -> IO ()
    loop inputState = do
      minput <- queryInput inputState (getInputLine "")
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do appendInputToLog input
                         let statement = words input
                         results <- case statement of
                           [] -> return []
                           _ -> exec inputState statement
                         case results of
                           [] -> main
                           _ -> do queryInput inputState $ mapM_ outputStrLn
                                     (results ++ [""])
                                   main

appendInputToLog :: String -> IO ()
appendInputToLog input =
  case input of
    "" -> return ()
    "\n" -> return ()
    _ -> appendFile "/Users/rose/Desktop/Dropbox/log.txt" (input ++ "\n")

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

runVim :: IO [String]
runVim = do
  exitSuccess <- system $ "vi /Users/rose/phil/temp"
  contents <- readFile "/Users/rose/phil/temp"
  add ProdDB Note $ wordsWithNewlines contents
  exitSuccess <- system $ "rm /Users/rose/phil/temp"
  return []

wordsWithNewlines :: String -> [String]
wordsWithNewlines input = 
  concat (map (\line -> case words line of 
                          [] -> ["\n"]
                          ws -> (init (words line)) 
                                 ++ [(last $ words line) ++ "\n"]) 
    (lines input))
  
-- | Shows the n lines most recently appended to the log
showLog :: Int -> IO [String]
showLog n = do
  file <- readFile "/Users/rose/Desktop/Dropbox/log.txt"
  return $ ["", ""] ++ (init $ lastN (n + 1) (lines file))

exec :: InputState -> [String] -> IO [String]
exec inputState (fn:args) = 
  case fn of
    "quit" -> exitSuccess
    "help" -> return (help)
    "todo" -> add ProdDB Todo args
    "note" -> add ProdDB Note args
    "fc" -> add ProdDB Flashcard args
    "goal" -> add ProdDB Goal args
    "done" -> do completeTodo ProdDB (read (head args) :: Int)
                 runLastGet ProdDB
    "log" -> showLog (read (head args) :: Int)
    "review" -> do result <- review ProdDB args
                   return [result]
    "e" -> do pipe <- sharedPipe  
              query <- getLastQueryForOne ProdDB (read (head args) :: Int)
              mdoc <- run pipe ProdDB $ findOne query
              case mdoc of
                Left failure -> do putStrLn $ show failure
                                   return []
                Right mDoc -> case mDoc of
                  Nothing -> do putStrLn "I couldn't find the document that you want to edit."
                                return []
                  Just doc -> do lastGet <- getLastGet ProdDB 
                                 let docType = getDocType $ head (words lastGet)
                                 edit ProdDB doc docType
                                 runLastGet ProdDB
    "vi" -> runVim
    "test" -> case (head args) of
      "goals" -> do
        docs <- getGoals ProdDB
        goalLoop inputState docs
      _ -> do
        incrementTestCount ProdDB args
        mTestCount <- getTestCount ProdDB args
        let testCount = case mTestCount of
              Nothing -> (-1 :: Int32)
              Just c -> c
        docs <- getFlashcards ProdDB args
        testLoop ProdDB inputState docs args testCount True True
    "g" -> get ProdDB args
    "d" -> do deleteItem ProdDB (read (head args) :: Int)
              runLastGet ProdDB
    tag -> add ProdDB Note (tag:args)

-- | Recursive. For each goal, print it, and get a y/n response
goalLoop :: InputState -> [Document] -> IO [String]
goalLoop inputState docs =
  case docs of
    doc:ds -> do
      let ObjId goalId = valueAt (labelStr ItemId) doc
          String text = valueAt (labelStr TextLabel) doc
      minput <- queryInput inputState (getInputLine $ (unpack text) ++ "\n\n")
      case minput of 
        Just "y" -> do
          scoreGoal ProdDB goalId 1
          goalLoop inputState ds
        Just "n" -> do
          scoreGoal ProdDB goalId 0
          goalLoop inputState ds
    [] -> getGoalScores ProdDB

showImage :: DatabaseName -> ObjectId -> DocLabel -> Document -> IO ()
showImage dbName questionId label doc = do
  pipe <- sharedPipe
  let query = select [(labelStr ItemId) =: questionId, 
        (labelStr label) =: ["$exists" =: True]] (docTypeToText Flashcard)
  mDoc <- run pipe dbName $ findOne query
  case mDoc of 
    Right doc -> case doc of 
      Nothing -> return ()
      Just d -> do 
        let String filenameStr = valueAt (labelStr label) d
            filename = unpack filenameStr
        exitSuccess <- system $ 
          "open /Users/rose/Desktop/flashcards/" ++ filename ++ ".png"
        return ()
    Left failure -> do putStrLn $ show failure

-- | Recursive. For each question, print it, and get a y/n response
testLoop :: DatabaseName -> InputState -> [Document] -> [String] -> Int32 -> 
  Bool -> Bool -> IO [String]
testLoop dbName inputState docs tags testCount isQuestion shouldPrintAnswer =
  case docs of
  doc:ds ->
    let ObjId questionId = valueAt (labelStr ItemId) doc
    in case isQuestion of
    True -> do
      putStrLn "--------------------------------------------------------------------------------\n"
      let String question = valueAt (labelStr Question) doc
      showImage dbName questionId QuestionImageFilename doc
      minput <- queryInput inputState (getInputLine $ (unpack question) ++ 
        "\n\n")
      testLoop dbName inputState docs tags testCount False True
    False -> do
      putStrLn ""
      let String answer = valueAt (labelStr Answer) doc
      showImage dbName questionId AnswerImageFilename doc
      let ans = if shouldPrintAnswer then ((unpack answer)) else ""
      minput <- queryInput inputState (getInputLine ans)
      case minput of
          Just "" -> answeredQuestionCorrectly dbName inputState ds tags
              testCount questionId
          Just "n" -> answeredQuestionIncorrectly dbName inputState ds
              tags testCount questionId
          Just "e" -> do edit ProdDB doc Flashcard
                         testLoop dbName inputState docs tags testCount False False
  [] -> showFlashcardScore ProdDB tags testCount
                        
answeredQuestionCorrectly :: DatabaseName -> InputState -> [Document] -> [String] -> Int32 -> 
  ObjectId -> IO [String]
answeredQuestionCorrectly dbName inputState docs tags testCount questionId = 
  do putStrLn "\n\n"
     addFlashcardScore ProdDB tags testCount questionId (1 :: Int32)
     testLoop dbName inputState docs tags testCount True True

answeredQuestionIncorrectly :: DatabaseName -> InputState -> [Document] -> [String] -> Int32 -> 
  ObjectId -> IO [String]
answeredQuestionIncorrectly dbName inputState docs tags testCount questionId =
  do putStrLn "\n\n"
     addFlashcardScore ProdDB tags testCount questionId (0 :: Int32)
     testLoop dbName inputState docs tags testCount True True

-- | Prints help message
help :: [String]
help = 
  [ "\n Available commands: "
  , "quit -- quit the REPL session"
  , "help - show this help message"
  , "\n"
  ]

migrate :: IO Pipe -> IO ()
migrate sharedPipe = do
  addDateCreatedToAllCollections sharedPipe ProdDB
  putStrLn "Done migrating."
