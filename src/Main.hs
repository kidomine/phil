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
import Control.Exception
import Data.Int
import Data.Text (unpack)
import Data.ConfigFile -- maybe  just ConfigFile
import Control.Monad.Error

import Utils
import Add
import Delete
import Get
import Migrate
import Review
import Validate
import Done
import Edit

data ConfigInfo = ConfigInfo { editpath :: String
                             , logpath :: String
                             , flashcardspath :: String
                             }

readConfig :: String -> IO ConfigInfo
readConfig f = do
   rv <- runErrorT $ do
      -- open the configuration file
      cp <- join $ liftIO $ readfile emptyCP f
      let x = cp
 
      -- read out the attributes
      ev <- Data.ConfigFile.get x "Default" "editpath"
      lv <- Data.ConfigFile.get x "Default" "logpath"
      fv <- Data.ConfigFile.get x "Default" "flashcardspath"
 
      -- build the config value
      return (ConfigInfo { editpath = ev
                         , logpath = lv
                         , flashcardspath = fv
                         })
 
   -- in the instance that configuration reading failed we'll
   -- fail the application here, otherwise send out the config
   -- value that we've built
   either (\x -> error (snd x)) (\x -> return x) rv

main :: IO ()
main = do
  config <- readConfig "/Users/rose/phil/src/phil.cfg"
  bracketOnError (initializeInput defaultSettings)
     cancelInput -- This will only be called if an exception such
                 -- as a SigINT is received.
     (\inputState -> loop inputState config >> closeInput inputState)
  where
    loop :: InputState -> ConfigInfo -> IO ()
    loop inputState config = do
      minput <- queryInput inputState (getInputLine "")
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do appendInputToLog input $ logpath config
                         let statement = words input
                         results <- case statement of
                           [] -> return []
                           _ -> exec inputState config statement
                         case results of
                           [] -> main
                           _ -> do queryInput inputState $ mapM_ outputStrLn
                                     (results ++ [""])
                                   main

appendInputToLog :: String -> String -> IO ()
appendInputToLog input logpath =
  case input of
    "" -> return ()
    "\n" -> return ()
    _ -> appendFile logpath (input ++ "\n")

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

runVim :: String -> IO [String]
runVim editpath = do
  exitSuccess <- system $ "vi " ++ editpath 
  contents <- readFile editpath
  add ProdDB Note $ wordsWithNewlines contents
  exitSuccess <- system $ "rm " ++ editpath 
  return []

wordsWithNewlines :: String -> [String]
wordsWithNewlines input = 
  concat (map (\line -> case words line of 
                          [] -> ["\n"]
                          ws -> (init (words line)) 
                                 ++ [(last $ words line) ++ "\n"]) 
    (lines input))
  
-- | Shows the n lines most recently appended to the log
showLog :: ConfigInfo -> Int -> IO [String]
showLog config n = do
  file <- readFile $ logpath config 
  return $ ["", ""] ++ (init $ lastN (n + 1) (lines file))

exec :: InputState -> ConfigInfo -> [String] -> IO [String]
exec inputState config (fn:args) = 
  case fn of
    "quit" -> exitSuccess
    "help" -> return (help)
    "todo" -> add ProdDB Todo args
    "t" -> add ProdDB Todo args
    "note" -> add ProdDB Note args
    "n" -> add ProdDB Note args
    "f" -> add ProdDB Flashcard args
    "fc" -> add ProdDB Flashcard args
    "done" -> do completeTodo ProdDB (read (head args) :: Int)
                 runLastGet ProdDB
    "log" -> showLog config (read (head args) :: Int)
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
    "vi" -> runVim $ editpath config
    "test" -> do 
      incrementTestCount ProdDB args
      mTestCount <- getTestCount ProdDB args
      let testCount = case mTestCount of
            Nothing -> (-1 :: Int32)
            Just c -> c
      docs <- getFlashcards ProdDB args
      testLoop ProdDB inputState config docs args testCount True True 1
    "g" -> Get.get ProdDB args
    "d" -> do deleteItem ProdDB (read (head args) :: Int)
              runLastGet ProdDB
    tag -> add ProdDB Note (tag:args)

showImage :: DatabaseName -> ConfigInfo -> ObjectId -> DocLabel -> Document -> IO ()
showImage dbName config questionId label doc = do
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
          "open " ++ (flashcardspath config) ++ filename ++ ".png"
        return ()
    Left failure -> do putStrLn $ show failure

-- | Recursive. For each question, print it, and get a y/n response
testLoop :: DatabaseName -> InputState -> ConfigInfo -> [Document] -> [String] -> 
  Int32 -> Bool -> Bool -> Int -> IO [String]
testLoop dbName inputState config docs tags testCount isQuestion shouldPrintAnswer flashcardNumber =
  case docs of
  doc:ds ->
    let ObjId questionId = valueAt (labelStr ItemId) doc
    in case isQuestion of
    True -> do
      putStrLn $ "----------------------------  " ++ (show flashcardNumber) ++ "  ----------------------------\n"
      let String question = valueAt (labelStr Question) doc
      showImage dbName config questionId QuestionImageFilename doc
      minput <- queryInput inputState (getInputLine $ (unpack question) ++ 
        "\n\n")
      testLoop dbName inputState config docs tags testCount False True flashcardNumber
    False -> do
      putStrLn ""
      let String answer = valueAt (labelStr Answer) doc
      showImage dbName config questionId AnswerImageFilename doc
      let ans = if shouldPrintAnswer then ((unpack answer) ++ "\n\n") else ""
      minput <- queryInput inputState (getInputLine ans)
      case minput of
          Just "n" -> answeredQuestionIncorrectly dbName inputState config ds
              tags testCount questionId (flashcardNumber + 1)
          Just "e" -> do edit ProdDB doc Flashcard
                         testLoop dbName inputState config docs tags testCount 
                           False False (flashcardNumber + 1)
          Just _ -> answeredQuestionCorrectly dbName inputState config ds tags
              testCount questionId (flashcardNumber + 1)
  [] -> showFlashcardScore ProdDB tags testCount
                        
answeredQuestionCorrectly :: DatabaseName -> InputState -> ConfigInfo -> [Document] -> 
  [String] -> Int32 -> ObjectId -> Int -> IO [String]
answeredQuestionCorrectly dbName inputState config docs tags testCount questionId flashcardNumber = 
  do addFlashcardScore ProdDB tags testCount questionId (1 :: Int32)
     testLoop dbName inputState config docs tags testCount True True flashcardNumber

answeredQuestionIncorrectly :: DatabaseName -> InputState -> ConfigInfo -> [Document] -> [String] -> Int32 -> 
  ObjectId -> Int -> IO [String]
answeredQuestionIncorrectly dbName inputState config docs tags testCount questionId flashcardNumber =
  do addFlashcardScore ProdDB tags testCount questionId (0 :: Int32)
     testLoop dbName inputState config docs tags testCount True True flashcardNumber

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
