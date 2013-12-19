module Main (
      main
    , getFieldsForTodo
    , todoIsValid
    , get
    , add
    , deleteAll
    , deleteItem
) where

import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Text (pack, unpack)
import Data.Int
import Data.Typeable
import Text.Printf
import Data.Char
import Database.MongoDB as DB
import Control.Monad.Trans (liftIO)
import System.Exit (exitSuccess)
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Control.Concurrent
import Control.Exception
import Data.Maybe (fromMaybe)

sharedPipe = runIOE $ connect (host "127.0.0.1")

run p dbName act = access p master (pack dbName) act

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
                                     [] -> return (Nothing)
                                     _ -> exec statement
                                 case results of 
                                    Nothing -> main
                                    Just strings -> do
                                         queryInput hd $ mapM_ outputStrLn strings
                                         main

exec :: [String] -> IO (Maybe [String])
exec (fn:args) = 
    case unpack (pack fn) of
        "quit" -> exitSuccess
        "help" -> return (Just help)
        "todo" -> add "db" "todo" (Just args)
        "g" -> get "db" args
        "d" -> deleteItem "db" args
        _ -> return (Just ["I don't recognize that command"])

-- | Prints help message
help :: [String]
help = 
    [ "\n Available commands: "
    , "quit -- quit the REPL session"
    , "help - show this help message"
    , "\n"
    ]
    
--
-- Parsing items
--

parseDoc :: Document -> String
parseDoc doc = do
    let String docTypeT = valueAt (pack "type") doc
    let docType = unpack docTypeT
    docType

parseDocs :: [Document] -> [String]
parseDocs docs 
    | docs == [] = []
    | otherwise = let texts = [text | String text <- map (valueAt (pack "text")) docs]
                      items = [unpack str | str <- texts]
                      numberedItems = zipWith (\n line -> show n ++ " - " ++ line) [1.. ] items
                  in numberedItems

remaining lst = case lst of
    x:[] -> Nothing
    x:xs -> Just xs
    _ -> Nothing

-- | Recursive function that builds up the selector based on args
constructSelector :: Selector -> Maybe [String] -> Selector 
constructSelector selector inputWords = case inputWords of
    Nothing -> selector 
    Just ws | ws == [] -> selector
            | (head (head ws)) == 'p' && isInteger (tail (head ws)) -> constructSelector (merge selector [(pack "priority") =: (read $ (tail (head ws)) :: Int32)]) $ remaining ws
    

constructSelection :: [String] -> Query
constructSelection args = let collection = pack $ head args
                              selector = constructSelector [] $ Just $ tail args
                          in select selector collection

get' :: IO Pipe -> String -> [String] -> IO (Maybe [String])
get' sharedPipe dbName args = do
    pipe <- liftIO sharedPipe
    let selection = constructSelection 
    cursor <- run pipe dbName $ DB.find (constructSelection args)
    docs <- run pipe dbName $ rest (case cursor of 
                                        Right c -> c)
    case docs of 
        Left _ -> return Nothing
        Right documents -> return $ Just $ parseDocs documents

get :: String -> [String] -> IO (Maybe [String])
get = get' sharedPipe

getFieldsForType docType ws = case docType of 
    "todo" -> getFieldsForTodo [] ws

isInteger :: String -> Bool
isInteger st
      | length st == 0 = False
      | length st == 1 = isNumber $ head st
      | otherwise = if isNumber (head st) == True then isInteger (tail st) else False 

-- ! Recursive function that builds up the document by merges
getFieldsForTodo doc inputWords = case inputWords of 
    Nothing -> doc
    Just ws | ws == [] -> doc
            | isUpper (head (head ws)) -> merge doc [(pack "text") =: unwords ws]
            | (head (head ws)) == 'p' && isInteger (tail (head ws)) -> getFieldsForTodo (merge doc [(pack "priority") =: (read $ (tail (head ws)) :: Int32)]) $ remaining ws
            | otherwise -> getFieldsForTodo (merge doc [(pack "tag") =: (pack $ head ws)]) (remaining ws)

--
-- Validating items
--

docIsValid :: String -> [String] -> Bool
docIsValid docType ws = case docType of
    "todo" -> todoIsValid ws

todoIsValid :: [String] -> Bool
todoIsValid ws = any (\word -> isUpper (head word)) ws

--
-- Adding items
-- 

add' :: IO Pipe -> String -> String -> Maybe [String] -> IO (Maybe [String])
add' sharedPipe dbName docType inputWords = do
    pipe <- sharedPipe
    let Just ws = inputWords
    if docIsValid docType ws 
        then do
            let doc = getFieldsForType docType (Just ws)
            e <- access pipe master (pack dbName) $ DB.insert (pack docType) doc
            case e of 
                Left _ -> return (Just ["Couldn't insert the note."])
                _ -> return Nothing
            return Nothing
        else do return Nothing


add :: String -> String -> Maybe [String] -> IO (Maybe [String])
add = add' sharedPipe

--
-- Deleting items
--

d' :: IO Pipe -> String -> [String] -> IO (Maybe [String])
d' sharedPipe dbName args = do
    pipe <- sharedPipe
    let docType = head args
        n = read (args !! 1) :: Int
    run pipe dbName $ DB.find (select [] (pack $ docType)) >>= rest >>= (del pipe docType n) 
    return Nothing

del :: Pipe -> String -> Int -> [Document] -> Action IO ()
del pipe docType n docs = do
    let collection = pack docType
    let ObjId itemId = valueAt (pack "_id") (docs !! (n-1)) 
    deleteOne (select [(pack "_id") =: itemId] collection)

deleteItem = d' sharedPipe

-- | Delete all documents of a certain type from db
deleteAll :: String -> String -> IO (Maybe [String])
deleteAll = deleteAll' sharedPipe

deleteAll' :: IO Pipe -> String -> String -> IO (Maybe [String])
deleteAll' sharedPipe dbName docType = do
    pipe <- sharedPipe
    e <- access pipe master (pack dbName) $ DB.delete (select [] (pack "todo"))
    return Nothing
