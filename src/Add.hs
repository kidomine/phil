{-# LANGUAGE OverloadedStrings #-}

module Add (
    add
  , getFieldsForTodo
  , tagIsNew
  , getQuestion
  , getAnswer
  , incrementTestCount
  , getTestCount
  , addScore
  , getQuestionScore
  , showTestScore
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
getFieldsForTodo :: Document -> [String] -> [String] -> Document
getFieldsForTodo doc inputWords tagsSoFar =
  case inputWords of
    [] -> case tagsSoFar of 
            [] -> doc 
            ts -> merge doc [(fieldToText Tags) =: ts]
    (firstLetter:tailLetters):tailWords
      | isUpper firstLetter ->
          getFieldsForTodo (merge doc [(fieldToText TextField) =: unwords 
            inputWords]) [] tagsSoFar
      | firstLetter == 'p' &&
          isInteger tailLetters ->
            getFieldsForTodo (merge doc [(fieldToText Priority) =: ((read 
              tailLetters) :: Int32)]) tailWords tagsSoFar
      | (firstLetter:tailLetters) == "by" ->
          getFieldsForTodo (merge doc [(fieldToText DueBy) =: (readDate (head 
            tailWords))]) (tail tailWords) tagsSoFar
      | otherwise -> getFieldsForTodo doc tailWords 
          (tagsSoFar ++ [firstWord])
            where firstWord = firstLetter:tailLetters

getFieldsForNote :: Document -> [String] -> [String] -> Document
getFieldsForNote doc inputWords tagsSoFar =
  case inputWords of
      [] -> case tagsSoFar of
              [] -> doc
              ts -> merge doc [(fieldToText Tags) =: ts]
      firstWord:tailWords | isUpper (head firstWord) ->
                              getFieldsForNote (merge doc 
                                [(fieldToText TextField)
                                  =: unwords inputWords])
                                    [] tagsSoFar
                          | otherwise -> getFieldsForNote 
                              doc tailWords (tagsSoFar ++ [firstWord])

getFieldsForEvent :: [String] -> Document
getFieldsForEvent inputWords = 
  case (splitDateTimeRangeTagsAndText $ unwords inputWords) of
    Just (day, startTime, endTime, tags, text) ->
      [(fieldToText StartDate) =: (UTCTime day startTime),
      (fieldToText EndDate) =: (UTCTime day endTime),
      (fieldToText Tags) =: tags,
      (fieldToText TextField) =: text]

getFieldsForGoal :: [String] -> Document
getFieldsForGoal inputWords =
  [(fieldToText TextField) =: (unwords inputWords)]

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
            ts -> merge doc [(fieldToText Tags) =: ts]
    firstWord:tailWords 
      | isUpper (head firstWord) ->
          getFieldsForFlashcard (merge doc [(fieldToText Question) =: 
            getQuestion (unwords inputWords), (fieldToText Answer) =: getAnswer
              (unwords inputWords)]) [] tagsSoFar
      | otherwise -> getFieldsForFlashcard
          doc tailWords (tagsSoFar ++ [firstWord])

tagIsNew :: DatabaseName -> DocType -> String -> IO Bool
tagIsNew dbName docType t =
  if (wordIsReserved t) then return False
  else do
    pipe <- sharedPipe
    let query = select [(fieldToText TypeField) =: (docTypeToText docType),
          (fieldToText TextField) =: t] (docTypeToText Tag)
    cursor <- run pipe dbName $ find query
    docs <- run pipe dbName $ rest (case cursor of Right c -> c)
    case docs of 
      Left failure -> do putStrLn $ show failure
                         return True
      Right documents | (length documents) == 0 -> return True
                      | otherwise -> return False

addNewTag :: DatabaseName -> DocType -> String -> IO ()
addNewTag dbName docType t = do
  pipe <- sharedPipe
  new <- tagIsNew dbName docType t
  if new 
      then do 
        now <- getCurrentTime
        let newDoc = [(fieldToText TextField) =: t, (fieldToText TypeField) =:
                       (docTypeToText docType), (fieldToText Created) =: now]
        run pipe dbName $ insert (docTypeToText Tag) newDoc
        return ()
      else
        return ()

add :: DatabaseName -> DocType -> [String] -> IO [String]
add dbName docType inputWords = do
  pipe <- sharedPipe
  if docIsValid docType inputWords
    then do
      mapM (addNewTag dbName docType) 
        (takeWhile (not . isUpper . head) inputWords)
      doc <- getFieldsForType docType inputWords 
      e <- run pipe dbName $ insert (docTypeToText docType) doc
      case e of
        Left failure -> do
          putStrLn $ show failure
          return []
        _ -> return []
      return []
    else do return []

getTestCount :: DatabaseName -> [String] -> IO (Maybe Int32)
getTestCount dbName tags = do
  pipe <- sharedPipe
  mdoc <- run pipe dbName $ findOne $ select [(fieldToText Tags) =: 
    ["$all" =: tags]]
      (docTypeToText TestCount)
  case mdoc of 
    Left failure -> do putStrLn $ show failure
                       return Nothing
    Right mDoc -> case mDoc of 
      Nothing -> return Nothing
      Just doc -> let Int32 count = valueAt 
                        (fieldToText Count) doc
                  in return $ Just count

incrementTestCount :: DatabaseName -> [String] -> IO ()
incrementTestCount dbName tags = do
  pipe <- sharedPipe
  let selection = (select [(fieldToText Tags) =: ["$all" =: tags]] 
        (docTypeToText TestCount))
  mCount <- getTestCount dbName tags
  case mCount of
      Nothing -> do 
        run pipe dbName $ insert (docTypeToText TestCount)
          [(fieldToText Tags) =: tags, (fieldToText Count) =: (1 :: Int32)] 
        return ()
      Just count -> do
        e <- run pipe dbName $ modify selection ["$inc" =: [(fieldToText Count) 
                =: (1 :: Int32)]]
        case e of
          Left _ -> error "Couldn't increment the test count"
          Right _ -> return ()

getQuestionScore :: DatabaseName -> [String] -> Int32 -> ObjectId -> IO Int32
getQuestionScore dbName tags testCount questionId = do
  pipe <- sharedPipe
  let selection = select ([(fieldToText Tags) =: ["$all" =: tags], 
        (fieldToText TestCountField) =: testCount, (fieldToText QuestionId) =: 
          questionId]) (docTypeToText Score)
  mResult <- run pipe dbName $ findOne selection
  case mResult of
    Left failure -> do putStrLn $ "Couldn't find the score.\n" ++ (show failure)
                       return 0
    Right mDoc -> case mDoc of
      Nothing -> error "Couldn't find the score"
      Just doc -> let Int32 scoreInt = valueAt (fieldToText ScoreField) doc
                  in return scoreInt

addScore :: DatabaseName -> [String] -> Int32 -> ObjectId -> Int32 -> IO ()
addScore dbName tags testCount questionId score = do
  pipe <- sharedPipe
  e <- run pipe dbName $ insert (docTypeToText Score) $
    [(fieldToText Tags) =: tags, (fieldToText TestCountField) =: testCount, 
      (fieldToText QuestionId) =: questionId, (fieldToText ScoreField) =: score]
  case e of 
    Left failure -> do putStrLn $ show failure
                       return ()
    Right score -> return ()

showTestScore :: DatabaseName -> [String] -> Int32 -> IO [String]
showTestScore dbName tags testCount = do
  pipe <- sharedPipe
  putStrLn $ "test count is " ++ show testCount
  let matchSelect = [(fieldToText TestCountField) =: testCount,
                     (fieldToText Tags) =: ["$all" =: tags] ]
  let pipeline = [ ["$match" =: matchSelect],
                      ["$group" =: ["_id" =: empty, "sum" =: 
                          ["$sum" =: ("$score" :: String)]]] ]
  docs <- run pipe dbName $ aggregate (docTypeToText Score) pipeline
  case docs of 
    Left failure -> do putStrLn $ show failure
                       return ["failed"]
    Right doc -> do let Int32 score = valueAt "sum" (head doc)
                    return ["score is " ++ show score] 
