{-# LANGUAGE OverloadedStrings #-}

module Add (
    add
  , getFieldsForTodo
  , getQuestion
  , getAnswer
  , incrementTestCount
  , getTestCount
  , addFlashcardScore
  , showFlashcardScore
) where

import Database.MongoDB
import Data.Text (empty)
import Data.Time
import Data.Char
import Data.Int

import Utils
import Validate

getFieldsForType :: DocType -> [String] -> IO Document
getFieldsForType docType inputWords = do
  time <- getCurrentTime
  let results = merge [] ["created" =: time] ++
        case docType of
          Todo -> getFieldsForTodo [] inputWords []
          Note -> getFieldsForNote [] inputWords []
          Flashcard -> getFieldsForFlashcard [] inputWords []
          Event -> getFieldsForEvent inputWords
          Goal -> getFieldsForGoal inputWords
  return results

-- ! Recursive function that builds up the document by merges
-- builds up list of tags until there are no tags left, then
-- sets the Tags field as the list of tags
-- will someday support:
--  tomorrow
--  in 3 weeks
--  in 2 months
--  by next Wednesday
--  by next year
--  by January
--  by Saturday
getFieldsForTodo :: Document -> [String] -> [String] -> Document
getFieldsForTodo doc inputWords tagsSoFar =
  case inputWords of
    [] -> case tagsSoFar of 
            [] -> doc 
            ts -> merge doc [(labelStr Tags) =: ts]
    (firstLetter:tailLetters):tailWords
      | isUpper firstLetter ->
          getFieldsForTodo (merge doc [(labelStr TextLabel) =: unwords 
            inputWords]) [] tagsSoFar
      | firstLetter == 'p' &&
          isInteger tailLetters ->
            getFieldsForTodo (merge doc [(labelStr Priority) =: ((read 
              tailLetters) :: Int32)]) tailWords tagsSoFar
      | (firstLetter:tailLetters) == "by" ->
          getFieldsForTodo (merge doc [(labelStr DueBy) =: (readDate (head 
            tailWords))]) (tail tailWords) tagsSoFar
      | otherwise -> getFieldsForTodo doc tailWords 
          (tagsSoFar ++ [firstWord])
            where firstWord = firstLetter:tailLetters

getFieldsForNote :: Document -> [String] -> [String] -> Document
getFieldsForNote doc inputWords tagsSoFar =
  case inputWords of
      [] -> case tagsSoFar of
              [] -> doc
              ts -> merge doc [(labelStr Tags) =: ts]
      firstWord:tailWords | isUpper (head firstWord) ->
                              getFieldsForNote (merge doc 
                                [(labelStr TextLabel)
                                  =: unwords inputWords])
                                    [] tagsSoFar
                          | otherwise -> getFieldsForNote 
                              doc tailWords (tagsSoFar ++ [firstWord])

getFieldsForEvent :: [String] -> Document
getFieldsForEvent inputWords = 
  case (splitDateTimeRangeTagsAndText $ unwords inputWords) of
    Just (day, startTime, endTime, tags, text) ->
      [(labelStr StartDate) =: (UTCTime day startTime),
      (labelStr EndDate) =: (UTCTime day endTime),
      (labelStr Tags) =: tags,
      (labelStr TextLabel) =: text]

getFieldsForGoal :: [String] -> Document
getFieldsForGoal inputWords =
  [(labelStr TextLabel) =: (unwords inputWords)]

getAnswer :: String -> String
getAnswer line = let (_, answerWithQuestionMark) = break (=='?') line
                 in case answerWithQuestionMark of
                      '?':' ':' ':answer -> answer
                      '?':' ':answer -> answer
                      '?':answer -> answer

getQuestion :: String -> String
getQuestion line = let (question, _) = break (=='?') line
                   in question

getFieldsForFlashcard :: Document -> [String] -> [String] -> Document
getFieldsForFlashcard doc inputWords tagsSoFar =
  case inputWords of
    [] -> case tagsSoFar of
            [] -> doc
            ts -> merge doc [(labelStr Tags) =: ts]
    firstWord:tailWords 
      | isUpper (head firstWord) ->
          getFieldsForFlashcard (merge doc [(labelStr Question) =: 
            getQuestion (unwords inputWords), (labelStr Answer) =: getAnswer
              (unwords inputWords)]) [] tagsSoFar
      | otherwise -> getFieldsForFlashcard
          doc tailWords (tagsSoFar ++ [firstWord])

add :: DatabaseName -> DocType -> [String] -> IO [String]
add dbName docType inputWords = do
  pipe <- sharedPipe
  if docIsValid docType inputWords
    then do
      doc <- getFieldsForType docType inputWords 
      e <- run pipe dbName $ insert_ (docTypeToText docType) doc
      case e of
        Left failure -> do
          putStrLn $ show failure
          return []
        Right () -> return []
      return []
    else do return []

getTestCount :: DatabaseName -> [String] -> IO (Maybe Int32)
getTestCount dbName tags = do
  pipe <- sharedPipe
  mdoc <- run pipe dbName $ findOne $ select [(labelStr Tags) =: 
    ["$all" =: tags]]
      (docTypeToText TestCount)
  case mdoc of 
    Left failure -> do putStrLn $ show failure
                       return Nothing
    Right mDoc -> case mDoc of 
      Nothing -> return Nothing
      Just doc -> let Int32 count = valueAt 
                        (labelStr Count) doc
                  in return $ Just count

incrementTestCount :: DatabaseName -> [String] -> IO ()
incrementTestCount dbName tags = do
  pipe <- sharedPipe
  let selection = (select [(labelStr Tags) =: ["$all" =: tags]] 
        (docTypeToText TestCount))
  mCount <- getTestCount dbName tags
  case mCount of
      Nothing -> do 
        run pipe dbName $ insert_ (docTypeToText TestCount)
          [(labelStr Tags) =: tags, (labelStr Count) =: (1 :: Int32)] 
        return ()
      Just count -> do
        e <- run pipe dbName $ modify selection ["$inc" =: [(labelStr Count) 
                =: (1 :: Int32)]]
        case e of
          Left _ -> error "Couldn't increment the test count"
          Right _ -> return ()

getFlashcardScore :: DatabaseName -> [String] -> Int32 -> ObjectId -> IO Int32
getFlashcardScore dbName tags testCount questionId = do
  pipe <- sharedPipe
  let selection = select ([(labelStr Tags) =: ["$all" =: tags], 
        (labelStr TestCountLabel) =: testCount, (labelStr QuestionId) =: 
          questionId]) (docTypeToText FlashcardScore)
  mResult <- run pipe dbName $ findOne selection
  case mResult of
    Left failure -> do putStrLn $ "Couldn't find the score.\n" ++ (show failure)
                       return 0
    Right mDoc -> case mDoc of
      Nothing -> error "Couldn't find the score"
      Just doc -> let Int32 scoreInt = valueAt (labelStr ScoreLabel) doc
                  in return scoreInt

addFlashcardScore :: DatabaseName -> [String] -> Int32 -> ObjectId -> Int32 -> 
  IO ()
addFlashcardScore dbName tags testCount questionId score = do
  pipe <- sharedPipe
  e <- run pipe dbName $ insert_ (docTypeToText FlashcardScore) $
    [(labelStr Tags) =: tags, (labelStr TestCountLabel) =: testCount, 
      (labelStr QuestionId) =: questionId, (labelStr ScoreLabel) =: score]
  case e of 
    Left failure -> do putStrLn $ show failure
                       return ()
    Right () -> return ()

showFlashcardScore :: DatabaseName -> [String] -> Int32 -> IO [String]
showFlashcardScore dbName tags testCount = do
  pipe <- sharedPipe
  putStrLn $ "Tested " ++ show testCount ++ " time" 
    ++ (if testCount == 1 then "" else "s")
  let matchSelect = [(labelStr TestCountLabel) =: testCount,
                     (labelStr Tags) =: ["$all" =: tags] ]
  let pipeline = [ ["$match" =: matchSelect],
                      ["$group" =: ["_id" =: empty, 
                          "scoreTotal" =: ["$sum" =: ("$score" :: String)],
                          "total" =: ["$sum" =: (1 :: Int32)]  ]]  ]
  mDocs <- run pipe dbName $ aggregate (docTypeToText FlashcardScore) pipeline
  case mDocs of 
    Left failure -> do putStrLn $ show failure
                       return ["failed"]
    Right docs -> do let Int32 score = valueAt "scoreTotal" (head docs)
                         Int32 outOf = valueAt "total" (head docs)
                     return ["score is " ++ (show score) ++ "/" ++ (show outOf)]
