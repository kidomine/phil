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
import Validate
import Delete
import Get

main :: IO ()
main = bracketOnError (initializeInput defaultSettings)
            cancelInput -- This will only be called if an exception such
                            -- as a SigINT is received.
            (\hd -> loop hd >> closeInput hd)
    where
        loop :: InputState -> IO ()
        loop hd = do
            minput <- queryInput hd (getInputLine "")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do let statement = words input
                                 results <- case statement of
                                     [] -> return []
                                     _ -> exec statement
                                 case results of 
                                    [] -> main
                                    _ -> do queryInput hd $ 
                                                mapM_ outputStrLn results
                                            main

exec :: [String] -> IO [String]
exec (fn:args) = 
    case unpack (pack fn) of
        "quit" -> exitSuccess
        "help" -> return (help)
        "todo" -> add ProdDB Todo args
        "note" -> add ProdDB Note args
        "g" -> get ProdDB args
        "d" -> deleteItem ProdDB args
        _ -> return ["I don't recognize that command"]

-- | Prints help message
help :: [String]
help = 
    [ "\n Available commands: "
    , "quit -- quit the REPL session"
    , "help - show this help message"
    , "\n"
    ]
