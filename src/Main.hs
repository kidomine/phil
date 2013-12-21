module Main (
      main
    , constructSelection
    , tagsSelector
    , getFieldsForTodo
    , noteIsValid
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
import Data.Time.Format.Human
import Data.Time

import Utils


data DocType = Todo | Tag | Cal | Note | Haha | Quote | People 
             | Goal | Survey | Question | Flashcard | Reminder
data DocField = TextField | TypeField | Priority | Tags | Created
data DatabaseName = ProdDB | TestDB

sharedPipe = runIOE $ connect (host "127.0.0.1")
run p dbName act = access p master (pack dbName) act
reservedWords = ["created"]

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
    
--
-- Preparing items for display
--

getFormattedDocs :: UTCTime -> [Document] -> [String] -> [String] -> [String]
getFormattedDocs currentTime docs args resultsSoFar = case docs of 
    [] -> []
    _ -> case resultsSoFar of -- the first time looping through, just 
                         -- create the numbers
            [] -> getFormattedDocs currentTime docs args formattedNumbers
                    where formattedNumbers = map (\n -> show n ++ " - ") 
                            (take (length docs) [1..])
            _ -> case args of 
                    [] -> let texts = [text | String text <- 
                                  map (valueAt (fieldToText TextField)) docs]
                              items = [unpack str | str <- texts]
                          in zipWith (++) resultsSoFar items
                    firstArg:tailArgs -> case firstArg of
                        "created" -> let bareDates = map (humanReadableTime' currentTime)
                                            [itemDate | UTC itemDate <- (map (valueAt (fieldToText Created)) docs)]
                                         dates = zipWith (++) bareDates (take (length bareDates) (repeat " - "))
                                         results = zipWith (++) resultsSoFar dates
                                     in getFormattedDocs currentTime docs tailArgs results
                        _ -> getFormattedDocs currentTime docs tailArgs resultsSoFar

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
    Created -> pack "created"

getDocField :: String -> DocField
getDocField string = case string of
    "tags" -> Tags
    "text" -> TextField
    "priority" -> Priority
    "created" -> Created

tagsSelector :: Selector -> [String] -> Selector
tagsSelector selector tags = case tags of
    [] -> selector
    t:ts -> tagsSelector (merge [] (selector ++ [(fieldToText Tags) =: t])) ts

-- | Recursive function that builds up the selector based on args
-- When there are no args left to examine, we check if we've 
-- recursively accumulated a list of tags
constructTodoSelector :: Selector -> [String] -> [String] -> Selector 
constructTodoSelector selector inputWords tagsSoFar =
    case inputWords of
        [] -> case tagsSoFar of 
            [] -> selector
            _ -> merge selector $ tagsSelector [] tagsSoFar
        firstWord:tailWords | isPriority firstWord ->
                                case firstWord of 
                                    firstLetter:restOfWord -> 
                                        constructTodoSelector (merge 
                                            selector [(fieldToText Priority) =: 
                                                (read restOfWord :: Int32)])
                                                    tailWords tagsSoFar
                            | firstWord `elem` reservedWords ->
                                constructTodoSelector selector tailWords tagsSoFar
                            | otherwise -> let newTags = case tagsSoFar of
                                                [] -> [firstWord]
                                                _ -> tagsSoFar ++ [firstWord]
                                           in constructTodoSelector 
                                              selector tailWords newTags

constructNoteSelector :: Selector -> [String] -> [String] -> Selector
constructNoteSelector selector inputWords tagsSoFar =
    case inputWords of
        [] -> case tagsSoFar of
            [] -> selector
            _ -> merge selector $ tagsSelector [] tagsSoFar
        firstWord:tailWords | firstWord `elem` reservedWords ->
                                 constructNoteSelector 
                                    selector tailWords tagsSoFar
                            | otherwise -> let newTags = case tagsSoFar of
                                                           [] -> [firstWord]
                                                           _ -> tagsSoFar ++ [firstWord]
                                           in constructNoteSelector selector 
                                                 tailWords newTags

constructSelection :: DocType -> [String] -> Query
constructSelection docType args = 
    let constructSelector = case docType of 
            Todo -> constructTodoSelector
            Note -> constructNoteSelector
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
                Right documents -> do 
                                     currentTime <- getCurrentTime
                                     return $ getFormattedDocs currentTime documents args []

get :: DatabaseName -> [String] -> IO [String]
get = get' sharedPipe

getFieldsForType :: DocType -> [String] -> IO Document
getFieldsForType docType inputWords = do
    time <- getCurrentTime
    return $ merge [] [(pack "created") =: time] ++ 
         case docType of 
            Todo -> getFieldsForTodo [] inputWords
            Note -> getFieldsForNote [] inputWords

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

getFieldsForNote :: Document -> [String] -> Document
getFieldsForNote doc inputWords = 
    case inputWords of 
    [] -> doc
    firstWord:tailWords | isUpper (head firstWord) -> 
                            merge doc [(fieldToText TextField) 
                                =: unwords inputWords]
                        | otherwise -> getFieldsForNote (merge [] doc ++ 
                            [(fieldToText Tags) =: pack firstWord]) tailWords

--
-- Validating items
--

docIsValid :: DocType -> [String] -> Bool
docIsValid docType inputWords = case docType of
    Note -> noteIsValid inputWords
    Todo -> noteIsValid inputWords

noteIsValid :: [String] -> Bool
noteIsValid inputWords = any 
    (\word -> isUpper (head word)) inputWords

todoIsValid = noteIsValid

--
-- Adding items
-- 

add' :: IO Pipe -> DatabaseName -> DocType -> [String] -> IO [String]
add' sharedPipe dbName docType inputWords = do
    pipe <- sharedPipe
    if docIsValid docType inputWords
        then do
            doc <- getFieldsForType docType inputWords
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

-- | Some strings are plural so I can e.g. type 'g notes'
-- When I expect many notes, typeing 'g note' feels wrong.
-- Haha check out the yin and yang below
getDocType :: String -> DocType
getDocType docType = case docType of
    "todo" -> Todo
    "todos" -> Todo
    "tag" -> Tag
    "tags" -> Tag
    "cal" -> Cal
    "rem" -> Reminder
    "note" -> Note
    "notes" -> Note
    "haha" -> Haha
    "quotes" -> Quote
    "survey" -> Survey
    "goals" -> Goal
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

-- TODO enable a d all rather than just d n
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
