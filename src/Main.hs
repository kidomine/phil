module Main (
      main
    , constructSelection
    , todoTagsSelector
    , getFieldsForTodo
    , todoIsValid
    , get
    , add
    , deleteAll
    , deleteItem
    , DatabaseName (TestDB)
    , DocType (..)
    , DocField (..)
    , fieldToText
    , docTypeToText
) where

import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Text (pack, unpack, Text)
import Data.Int
import Data.Char
import Database.MongoDB as DB
import Control.Monad.Trans (liftIO)
import System.Exit (exitSuccess)
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Control.Concurrent
import Control.Exception
import Data.Maybe (fromMaybe)
import Utils

data DocType = Todo | Tag | Cal | Note | Haha | Quote | People 
             | Goal | Survey | Question | Flashcard | Reminder

data DocField = TextField | TypeField | Priority | Tags

data DatabaseName = ProdDB | TestDB

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
    
--
-- Parsing items
--

parseDoc :: Document -> String
parseDoc doc = do
    let String docTypeT = valueAt (fieldToText TypeField) doc
    let docType = unpack docTypeT
    docType

parseDocs :: [Document] -> [String]
parseDocs docs = case docs of 
    [] -> []
    _ -> let texts = [text | String text <- 
                          map (valueAt (fieldToText TextField)) docs]
             items = [unpack str | str <- texts]
             numberedItems = zipWith (\n line -> 
                  show n ++ " - " ++ line) [1.. ] items
         in numberedItems

isPriority :: String -> Bool
isPriority w = case w of
    ('p':restOfWord) ->
       isInteger restOfWord
    _ -> False

fieldToText :: DocField -> Text
fieldToText field = case field of
    Tags -> pack "tags"
    TextField -> pack "text"
    TypeField -> pack "type"
    Priority -> pack "priority"

getDocField :: String -> DocField
getDocField string = case string of
    "tags" -> Tags
    "text" -> TextField
    "priority" -> Priority

todoTagsSelector :: Selector -> [String] -> Selector
todoTagsSelector selector tags = case tags of
    [] -> selector
    t:ts -> todoTagsSelector 
        (merge [] (selector ++ [(fieldToText Tags) =: t])) ts

-- | Recursive function that builds up the selector based on args
-- When there are no args left to examine, we check if we've 
-- recursively accumulated a list of tags
constructTodoSelector :: Selector -> [String] -> [String] -> Selector 
constructTodoSelector selector inputWords tagsSoFar =
    case inputWords of
        [] -> case tagsSoFar of 
            [] -> selector
            _ -> merge selector $ todoTagsSelector [] tagsSoFar
        firstWord:tailWords -> 
            if isPriority firstWord then
                case firstWord of 
                    firstLetter:restOfWord -> 
                        constructTodoSelector (merge 
                            selector [(fieldToText Priority) =: 
                                (read restOfWord :: Int32)])
                                    tailWords tagsSoFar
            else let newTags = case tagsSoFar of
                        [] -> [firstWord]
                        _ -> tagsSoFar ++ [firstWord]
                 in constructTodoSelector selector tailWords newTags


constructSelection :: DocType -> [String] -> Query
constructSelection docType args = 
    let constructSelector = case docType of 
            Todo -> constructTodoSelector
    in case args of 
        [] -> select [] (docTypeToText docType)
        ws -> select (constructSelector [] ws []) 
            (docTypeToText docType)

databaseNameToString :: DatabaseName -> String
databaseNameToString dbName = case dbName of 
    ProdDB -> "db"
    TestDB -> "testDB"
    
get' :: IO Pipe -> DatabaseName -> [String] -> IO [String]
get' sharedPipe dbName arguments = do
    pipe <- liftIO sharedPipe
    case arguments of 
        docTypeArg:args -> do
            let docType = getDocType docTypeArg
                selection = constructSelection docType args
            cursor <- run pipe (databaseNameToString dbName) 
                $ DB.find selection
            docs <- run pipe (databaseNameToString dbName) 
                $ rest (case cursor of 
                     Right c -> c)
            case docs of 
                Left _ -> return []
                Right documents -> return $ parseDocs documents

get :: DatabaseName -> [String] -> IO [String]
get = get' sharedPipe

getFieldsForType :: DocType -> [String] -> Document
getFieldsForType docType inputWords = case docType of 
    Todo -> getFieldsForTodo [] inputWords

-- ! Recursive function that builds up the document by merges
getFieldsForTodo :: Document -> [String] -> Document
getFieldsForTodo doc inputWords =
    case inputWords of 
        [] -> doc
        (firstLetter:tailLetters):tailWords 
            | isUpper firstLetter ->
                merge doc [(fieldToText TextField) =: unwords inputWords]
            | firstLetter == 'p' && 
                isInteger tailLetters ->
                    getFieldsForTodo (merge doc 
                        [(fieldToText Priority) =: 
                            ((read tailLetters) :: Int32)])
                                tailWords
            | otherwise -> getFieldsForTodo (merge [] doc ++ 
                [(fieldToText Tags) =: 
                    (pack firstWord)]) tailWords
                        where firstWord = firstLetter:tailLetters

--
-- Validating items
--

docIsValid :: DocType -> [String] -> Bool
docIsValid docType inputWords = case docType of
    Todo -> todoIsValid inputWords

todoIsValid :: [String] -> Bool
todoIsValid inputWords = any 
    (\word -> isUpper (head word)) inputWords

--
-- Adding items
-- 

add' :: IO Pipe -> DatabaseName -> DocType -> [String] -> IO [String]
add' sharedPipe dbName docType inputWords = do
    pipe <- sharedPipe
    if docIsValid docType inputWords
        then do
            case docType of
                Todo -> do 
                    let doc = getFieldsForType docType inputWords
                    e <- access pipe master (pack $ 
                        databaseNameToString dbName) 
                            $ DB.insert (docTypeToText docType) doc
                    case e of 
                        Left _ -> return ["Couldn't insert the note."]
                        _ -> return []
                    return []
        else do return []


add :: DatabaseName -> DocType -> [String] -> IO ([String])
add = add' sharedPipe

-- | Haha check out the yin and yang below
getDocType :: String -> DocType
getDocType docType = case docType of
    "todo" -> Todo
    "tag" -> Tag
    "cal" -> Cal
    "rem" -> Reminder
    "note" -> Note
    "haha" -> Haha
    "quote" -> Quote
    "survey" -> Survey
    "goal" -> Goal
    "fc" -> Flashcard
    "q" -> Question

docTypeToText :: DocType -> Text
docTypeToText docType = case docType of
    Todo -> pack "todo"
    Tag -> pack "tag"
    Cal -> pack "cal"
    Note -> pack "note"
    Haha -> pack "haha"
    Quote -> pack "quote"
    Survey -> pack "survey"
    Question -> pack "q"
    Flashcard -> pack "fc"
    Reminder -> pack "rem"
    Goal -> pack "goal"

--
-- Deleting items
--

d' :: IO Pipe -> DatabaseName -> [String] -> IO [String]
d' sharedPipe dbName args = do
    pipe <- sharedPipe
    let docType = getDocType $ head args
        n = read (args !! 1) :: Int
    run pipe (databaseNameToString dbName) $ 
        DB.find (select [] (docTypeToText docType)) 
            >>= rest >>= (del pipe docType n) 
    return []

del :: Pipe -> DocType -> Int -> [Document] -> Action IO ()
del pipe docType n docs = do
    let collection = docTypeToText docType
    let ObjId itemId = valueAt (pack "_id") (docs !! (n-1)) 
    deleteOne (select [(pack "_id") =: itemId] collection)

deleteItem = d' sharedPipe

-- | Delete all documents of a certain type from db
deleteAll :: DatabaseName -> DocType -> IO [String]
deleteAll = deleteAll' sharedPipe

deleteAll' :: IO Pipe -> DatabaseName -> DocType -> IO [String]
deleteAll' sharedPipe dbName docType = do
    pipe <- sharedPipe
    e <- access pipe master (pack $ databaseNameToString dbName) 
        $ DB.delete (select [] (docTypeToText docType))
    return []
