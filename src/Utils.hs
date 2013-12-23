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
    , wordIsReserved
    , beginningOfTime
    , readDate
) where

import Data.Char
import Data.Text (pack, Text)
import Data.List (isInfixOf)
import Data.Time
import Database.MongoDB

data DocType = Todo | Tag | Cal | Note | Haha | Quote | People
             | Goal | Survey | Flashcard | Reminder | Score | TestCount
data DocField = TextField | TypeField | Priority | Tags | Created
                | DueBy | Question | Answer | Count
data DatabaseName = ProdDB | TestDB

sharedPipe = runIOE $ connect (host "127.0.0.1")
run p dbName act = access p master (pack $ databaseNameToString dbName) act
reservedWords = ["created", "tags", 
                 "today", "yesterday", "tomorrow", "by",
                 "with"] 
beginningOfTime = UTCTime (fromGregorian 2014 1 1) 
    (timeOfDayToTime $ TimeOfDay 0 0 0)

wordIsReserved :: String -> Bool
wordIsReserved word = (word `elem` reservedWords) || (isInfixOf "/" word)

isInteger :: String -> Bool
isInteger st
    | length st == 0 = False
    | length st == 1 = isNumber $ head st
    | otherwise = case st of
        firstChar:tailChars ->
            if (isNumber firstChar) == True
            then (isInteger tailChars) else False

readDate :: String -> UTCTime
readDate string = let (month, day) = break (=='/') string
                      monthNumber = read month :: Int
                      dayNumber = read (tail day) :: Int
                  in UTCTime (fromGregorian 2014 monthNumber dayNumber)
                      (timeOfDayToTime $ TimeOfDay 0 0 0)

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
    DueBy -> pack "dueBy"
    Question -> pack "question"
    Answer -> pack "answer"
    Count -> pack "count"

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

docTypeToText :: DocType -> Text
docTypeToText docType = case docType of
    Todo -> pack "todo"
    Tag -> pack "tag"
    Cal -> pack "cal"
    Note -> pack "note"
    Haha -> pack "haha"
    Quote -> pack "quote"
    Survey -> pack "survey"
    Flashcard -> pack "fc"
    Reminder -> pack "rem"
    Goal -> pack "goal"
    Score -> pack "score"
    TestCount -> pack "testCount"
