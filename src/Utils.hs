{-# LANGUAGE OverloadedStrings #-}

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
  , isTimeRange
  , isSlashDate
  , splitDateTimeRangeTagsAndText
) where

import Data.Char
import Data.Text (pack, Text)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, inits, tails, stripPrefix)
import Data.Time
import Database.MongoDB

data DocType = Todo | Tag | Event | Note | Goal | Flashcard 
               | Reminder | Score | TestCount | GoalScore
data DocField = TextField | TypeField | Priority | Tags | Created
                | DueBy | Question | Answer | Count | ItemId | QuestionId
                | TestCountField | ScoreField | StartDate | EndDate | GoalId
                | Done | Updated
data DatabaseName = ProdDB | TestDB

sharedPipe = runIOE $ connect (host "127.0.0.1")
run p dbName act = access p master (pack $ databaseNameToString dbName) act
reservedWords = ["created", "tags", 
                 "today", "yesterday", "tomorrow", "by",
                 "with", "done"] 
beginningOfTime = UTCTime (fromGregorian 2014 1 1) 
    (timeOfDayToTime $ TimeOfDay 0 0 0)

wordIsReserved :: String -> Bool
wordIsReserved word = (word `elem` reservedWords) || (isInfixOf "/" word)

-- | Check whether the string can be broken into Int/Int
isSlashDate :: String -> Bool
isSlashDate string = (isInfixOf "/" string) &&
  let (month, slashDay) = break (=='/') string
  in (isInteger month) && (isInteger $ tail slashDay)

slashDateToDay :: String -> Day
slashDateToDay string =
  let (month, slashDay) = break (=='/') string
  in fromGregorian 2014 (read month :: Int) (read (tail slashDay) :: Int)

getHoursAndSeconds :: String -> (Int, Int)
getHoursAndSeconds string =
  let plus12 = if (drop ((length time) - 2) time) == "pm" then 12 else 0
      time = timeSansAmOrPm string
  in case splitAboutSubstring time ":" of
       Nothing -> ((read time :: Int) + plus12, 0)
       Just (hour, minute) -> ((read hour :: Int) + plus12, 
         (read minute :: Int))

getTimeRange :: String -> (DiffTime, DiffTime)
getTimeRange string =
  let (startString, endString) =  
        case splitAboutSubstring string " - " of
          Just (start, end) -> (start, end)
      (startHours, startMinutes) = getHoursAndSeconds startString
      startSeconds = (startHours * 60 + startMinutes) * 60 
      (endHours, endMinutes) = getHoursAndSeconds endString
      endSeconds = (endHours * 60 + endMinutes) * 60 
  in (secondsToDiffTime (toInteger startSeconds), secondsToDiffTime (toInteger
        endSeconds))

isMinuteInt :: Int -> Bool
isMinuteInt minute = minute >= 0 && minute <= 60

isHourInt :: Int -> Bool
isHourInt hour = hour > 0 && hour <= 12

timeSansAmOrPm :: String -> String
timeSansAmOrPm time 
  | ((drop ((length time) - 2) time) == "am" || (drop ((length time) - 2) time)
      == "pm") = take ((length time) - 2) time
  | otherwise = time

isDigitTime :: (Int -> Bool) -> String -> Bool
isDigitTime func time
    | isInteger t = func (read t :: Int)
    | otherwise = False
  where t = timeSansAmOrPm time

splitDateAndRest :: String -> Maybe (Day, String)
splitDateAndRest string =
  if any isSlashDate (inits string)
    then
      let dateString = last $ filter isSlashDate (inits string)
      in Just $ (slashDateToDay dateString, drop 
                ((length dateString) + 1) string) -- also drops leading space
  else Nothing

-- | Takes in a string like "12/26 4:30pm - 7pm tag1 This"
-- and returns the day, startTime, startTime and "tag1 This"
splitDateTimeRangeTagsAndText :: String -> 
  Maybe (Day, DiffTime, DiffTime, [String], String)
splitDateTimeRangeTagsAndText string =
  case splitDateAndRest string of 
    Nothing -> Nothing
    Just (date, restOfString) -> 
      if any isTimeRange (inits restOfString) then
          let time = last $ filter isTimeRange (inits restOfString)
              (startTime, endTime) = getTimeRange time
              remainingString = drop ((length time) + 1) restOfString
              (tagsString, text) = break isUpper remainingString
          in Just (date, startTime, endTime, words tagsString, text)
       else Nothing

isTime12 :: String -> Bool
isTime12 time = case splitAboutSubstring time ":" of
  Nothing | isInteger hour && ((read hour) :: Int) > 0 
              && ((read hour) :: Int) <= 12 -> True
          | otherwise -> False
        where 
          hour = time
  Just (hour, minute) -> isDigitTime isHourInt hour && isDigitTime isMinuteInt minute

isTime :: String -> Bool
isTime time 
  | (drop ((length time) - 2) time) == "am" = 
      isTime $ take ((length time) - 2) time
  | (drop ((length time) - 2) time) == "pm" =
      isTime $ take ((length time) - 2) time
  | otherwise = isTime12 time

stripSuffix :: String -> String -> String
stripSuffix suffix string = take ((length string) - (length suffix)) string

-- | Splits before and after the first instance of substring in string
splitAboutSubstring :: String -> String -> Maybe (String, String)
splitAboutSubstring string substring
    | (((length starts) == 0) || ((length ends) == 0)) = Nothing
    | otherwise = Just (stripSuffix substring (head starts), end)
  where
    starts = filter (isSuffixOf substring) (inits string)
    ends = filter (isPrefixOf substring) (tails string)
    end = case (stripPrefix substring (head ends)) of
      Nothing -> ""
      Just e -> e

isTimeRange :: String -> Bool
isTimeRange timeRange = 
  case (splitAboutSubstring timeRange " - ") of
    Nothing -> False
    Just (start, end) -> isTime start && isTime end

isInteger :: String -> Bool
isInteger st
  | length st == 0 = False
  | length st == 1 = isNumber $ head st
  | otherwise = case st of
      firstChar:tailChars ->
          if (isNumber firstChar) == True
          then (isInteger tailChars) else False

readDate :: String -> UTCTime
readDate string = 
  let (month, day) = break (=='/') string
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
  ScoreField -> pack "score"
  ItemId -> pack "_id"
  QuestionId -> pack "questionId"
  GoalId -> pack "goalId"
  TestCountField -> pack "testCount"
  StartDate -> pack "startDate"
  EndDate -> pack "endDate"
  Done -> pack "done"
  Updated -> pack "updated"

-- | Some strings are plural so I can e.g. type 'g notes'
-- When I expect many notes, typeing 'g note' feels wrong.
-- Haha check out the yin and yang below
getDocType :: String -> DocType
getDocType docType = case docType of
  "todo" -> Todo
  "todos" -> Todo
  "tag" -> Tag
  "tags" -> Tag
  "rem" -> Reminder
  "note" -> Note
  "notes" -> Note
  "goals" -> Goal
  "goal" -> Goal
  "fc" -> Flashcard
  "event" -> Event

docTypeToText :: DocType -> Text
docTypeToText docType = case docType of
  Todo -> pack "todo"
  Tag -> pack "tag"
  Note -> pack "note"
  Event -> pack "event"
  Flashcard -> pack "fc"
  Reminder -> pack "rem"
  Goal -> pack "goal"
  Score -> pack "score"
  TestCount -> pack "testCount"
  GoalScore -> pack "goalScore"
