module Main (
      main
) where

import System.Environment
import System.Directory
import System.IO
import Data.List (init, foldl', concat)
import Data.Text (pack, unpack)
import Database.MongoDB 
import System.Exit (exitSuccess)
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Control.Concurrent
import Control.Exception
import Data.Int

import System.Process

import Utils
import Add
import Delete
import Get
import Migrate
import Review
import Validate
import Goals
import Done

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
                                    _ -> do queryInput inputState $ 
                                                mapM_ outputStrLn 
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
    case unpack (pack fn) of
        "quit" -> exitSuccess
        "help" -> return (help)
        "todo" -> add ProdDB Todo args
        "note" -> add ProdDB Note args
        "fc" -> add ProdDB Flashcard args
        "goal" -> add ProdDB Goal args
        "done" -> completeTodo ProdDB (read (head args) :: Int)
        "log" -> showLog (read (head args) :: Int)
        "review" -> do 
                      result <- review ProdDB args
                      return [result]
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
                testLoop inputState docs args testCount True
        "g" -> get ProdDB args
        "d" -> do deleteItem ProdDB args
                  get ProdDB [(head args)]
        _ | eventIsValid (fn:args) -> add ProdDB Event (fn:args)
          | otherwise -> return ["I don't recognize that command"]

-- | Recursive. For each goal, print it, and get a y/n response
goalLoop :: InputState -> [Document] -> IO [String]
goalLoop inputState docs =
  case docs of
    doc:ds -> do
      let ObjId goalId = valueAt (fieldToText ItemId) doc
          String text = valueAt (fieldToText TextField) doc
      minput <- queryInput inputState (getInputLine $ (unpack text) ++ "\n\n")
      case minput of 
        Just "y" -> do
                      scoreGoal ProdDB goalId 1
                      goalLoop inputState ds
        Just "n" -> do
                      scoreGoal ProdDB goalId 0
                      goalLoop inputState ds
    [] -> getGoalScores ProdDB

-- | Recursive. For each question, print it, and get a y/n response
testLoop :: InputState -> [Document] -> [String] -> Int32 -> Bool -> IO [String]
testLoop inputState docs tags testCount isQuestion =
    case docs of
    doc:ds -> 
        let ObjId questionId = valueAt (fieldToText ItemId) doc
        in case isQuestion of
        True -> do 
                    putStrLn "Question\n-----------"
                    let String question = valueAt (fieldToText Question) doc
                    minput <- queryInput inputState (getInputLine 
                        $ (unpack question) ++ "\n\n")
                    testLoop inputState docs tags testCount False
        False -> do 
                    putStrLn "Answer\n-----------"
                    let String answer = valueAt (fieldToText Answer) doc
                    minput <- queryInput inputState (getInputLine 
                        $ (unpack answer) ++ "\n\n")
                    case minput of
                        Just "y" -> answeredQuestionCorrectly inputState ds tags
                            testCount questionId
                        Just "n" -> answeredQuestionIncorrectly inputState ds
                            tags testCount questionId
    [] -> showTestScore ProdDB tags testCount
                        
answeredQuestionCorrectly :: InputState -> [Document] -> [String] -> Int32 -> 
  ObjectId -> IO [String]
answeredQuestionCorrectly inputState docs tags testCount questionId = 
  do putStrLn "\n\n"
     addScore ProdDB tags testCount questionId (1 :: Int32)
     testLoop inputState docs tags testCount True

answeredQuestionIncorrectly :: InputState -> [Document] -> [String] -> Int32 -> 
  ObjectId -> IO [String]
answeredQuestionIncorrectly inputState docs tags testCount questionId =
  do putStrLn "\n\n"
     addScore ProdDB tags testCount questionId (0 :: Int32)
     testLoop inputState docs tags testCount True

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
