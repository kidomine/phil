module Utils (
      isInteger
    , databaseNameToString
    , fieldToText
    , getDocType
    , docTypeToText
    , DatabaseName (..)
    , DocType (..)
    , DocField (..)
    , sharedPipe
    , run
    , reservedWords
) where

import Data.Char
import Data.Text (pack, Text)
import Database.MongoDB

data DocType = Todo | Tag | Cal | Note | Haha | Quote | People
             | Goal | Survey | Question | Flashcard | Reminder
data DocField = TextField | TypeField | Priority | Tags | Created
data DatabaseName = ProdDB | TestDB

sharedPipe = runIOE $ connect (host "127.0.0.1")
run p dbName act = access p master (pack dbName) act
reservedWords = ["created"]
{-
 -- why does this become wrapped in an Action?
allCollections = [Todo, Tag, Cal, Note, Haha, Quote,
                  People, Goal, Survey, Question,
                  Flashcard, Reminder]
                  -}

isInteger :: String -> Bool
isInteger st
    | length st == 0 = False
    | length st == 1 = isNumber $ head st
    | otherwise = case st of
        firstChar:tailChars ->
            if (isNumber firstChar) == True
            then (isInteger tailChars) else False

databaseNameToString :: DatabaseName -> String
databaseNameToString dbName = case dbName of
    ProdDB -> "db"
    TestDB -> "testDB"

fieldToText :: DocField -> Text
fieldToText field = case field of
    Tags -> pack "tags"
    TextField -> pack "text"
    TypeField -> pack "type"
    Priority -> pack "priority"
    Created -> pack "created"

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
