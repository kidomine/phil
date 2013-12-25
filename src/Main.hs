module Main (
      main
) where

import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Text (pack, unpack)
import Database.MongoDB 
import System.Exit (exitSuccess)
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Control.Concurrent
import Control.Exception
import Data.Int

import Utils
import Add
import Delete
import Get
import Migrate
import Review
import Validate
import Goals

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
                Just input -> do let statement = words input
                                 results <- case statement of
                                     [] -> return []
                                     _ -> exec inputState statement
                                 case results of 
                                    [] -> main
                                    _ -> do queryInput inputState $ 
                                                mapM_ outputStrLn 
                                                    (results ++ [""])
                                            main

exec :: InputState -> [String] -> IO [String]
exec inputState (fn:args) = 
    case unpack (pack fn) of
        "quit" -> exitSuccess
        "help" -> return (help)
        "todo" -> add ProdDB Todo args
        "note" -> add ProdDB Note args
        "fc" -> add ProdDB Flashcard args
        "goal" -> add ProdDB Goal args
        "review" -> do 
                      result <- review ProdDB args
                      return [result]
        "test" -> case (head args) of
              "goals" -> testGoals
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
                        Just "" -> answeredQuestionCorrectly inputState ds tags
                            testCount questionId
                        Just "n" -> answeredQuestionIncorrectly inputState docs 
                            tags testCount questionId
    [] -> showTestScore ProdDB testCount
                        
answeredQuestionCorrectly :: InputState -> [Document] -> [String] -> Int32 -> 
    ObjectId -> IO [String]
answeredQuestionCorrectly inputState docs tags testCount questionId = 
    do putStrLn "\n\n"
       addScore ProdDB tags testCount
           questionId (1 :: Int32)
       testLoop inputState docs tags testCount 
           True

answeredQuestionIncorrectly :: InputState -> [Document] -> [String] -> Int32 -> 
    ObjectId -> IO [String]
answeredQuestionIncorrectly inputState docs tags testCount questionId =
    do putStrLn "\n\n"
       addScore ProdDB tags testCount 
           questionId (0 :: Int32)
       testLoop inputState docs tags testCount 
           True

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
