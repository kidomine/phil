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

import Utils
import Add
import Delete
import Get
import Migrate

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
        "fc" -> do add ProdDB Flashcard args
        --"review" -> 
        {-
        "test" -> do 
                   minput <- queryInput inputState (getInputLine "(y|n) "
                   case minput of
                       Nothing -> return ()
                       Just "y" -> 
                       -}
        "g" -> get ProdDB args
        "d" -> do deleteItem ProdDB args
                  get ProdDB [(head args)]
        _ -> return ["I don't recognize that command"]

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
