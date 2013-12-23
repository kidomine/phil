module Add (
      add
    , getFieldsForTodo
    , tagIsNew
    , getQuestion
    , getAnswer
) where

import Database.MongoDB
import Data.Text (pack, Text)
import Data.Time
import Data.Char
import Data.Int

import Utils
import Validate

getFieldsForType :: DocType -> [String] -> IO Document
getFieldsForType docType inputWords = do
    time <- getCurrentTime
    let results = merge [] [(pack "created") =: time] ++
         case docType of
            Todo -> getFieldsForTodo [] inputWords []
            Note -> getFieldsForNote [] inputWords []
            Flashcard -> getFieldsForFlashcard [] inputWords []
    return results

-- ! Recursive function that builds up the document by merges
-- builds up list of tags until there are no tags left, then
-- sets the Tags field as the list of tags
getFieldsForTodo :: Document -> [String] -> [Text] -> Document
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
                    getFieldsForTodo (merge doc
                        [(fieldToText Priority) =:
                            ((read tailLetters) :: Int32)])
                                tailWords tagsSoFar
            | (firstLetter:tailLetters) == "by" ->
                getFieldsForTodo (merge doc 
                    [(fieldToText DueBy) =: (readDate (head tailWords))])
                        (tail tailWords) tagsSoFar
            | otherwise -> getFieldsForTodo doc tailWords 
                (tagsSoFar ++ [pack firstWord])
                        where firstWord = firstLetter:tailLetters

getFieldsForNote :: Document -> [String] -> [Text] -> Document
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
                                doc tailWords (tagsSoFar ++ [pack firstWord])

getAnswer :: String -> String
getAnswer line = let (_, answerWithQuestionMark) = break (=='?') line
                 in case answerWithQuestionMark of
                        '?':' ':' ':answer -> answer
                        '?':' ':answer -> answer
                        '?':answer -> answer

getQuestion :: String -> String
getQuestion line = let (question, _) = break (=='?') line
                   in question

getFieldsForFlashcard :: Document -> [String] -> [Text] -> Document
getFieldsForFlashcard doc inputWords tagsSoFar =
    case inputWords of
        [] -> case tagsSoFar of
                [] -> doc
                ts -> merge doc [(fieldToText Tags) =: ts]
        firstWord:tailWords | isUpper (head firstWord) ->
                                getFieldsForFlashcard (merge doc 
                                    [(fieldToText Question)
                                        =: getQuestion (unwords inputWords),
                                     (fieldToText Answer)
                                        =: getAnswer (unwords inputWords)])
                                            [] tagsSoFar
                            | otherwise -> getFieldsForFlashcard
                                doc tailWords (tagsSoFar ++ [pack firstWord])

tagIsNew :: Pipe -> DatabaseName -> DocType -> String -> IO Bool
tagIsNew pipe dbName docType t =
    if (wordIsReserved t) then return False
    else do
        cursor <- run pipe dbName $ 
            find $ select [(fieldToText TypeField) =: (docTypeToText docType),
                  (fieldToText TextField) =: (pack t)] (docTypeToText Tag)
        docs <- run pipe dbName $ rest (case cursor of Right c -> c)
        case docs of 
            Left _ -> return True -- todo raise an error instead
            Right documents | (length documents) == 0 -> return True
                            | otherwise -> return False

addNewTag :: Pipe -> DatabaseName -> DocType -> String -> IO ()
addNewTag pipe dbName docType t = do
    new <- tagIsNew pipe dbName docType t
    if new 
        then do now <- getCurrentTime
                e <- run pipe dbName $ insert (docTypeToText Tag) $
                        [(fieldToText TextField) =: pack t,
                         (fieldToText TypeField) =: 
                            (docTypeToText docType),
                         (fieldToText Created) =: now]
                return ()
        else
            return ()

add :: DatabaseName -> DocType -> [String] -> IO [String]
add dbName docType inputWords = do
    pipe <- sharedPipe
    if docIsValid docType inputWords
        then do
            mapM (addNewTag pipe dbName docType) 
                (takeWhile (\wrd -> not $ isUpper (head wrd)) inputWords)
            doc <- getFieldsForType docType inputWords 
            e <- run pipe dbName $ insert (docTypeToText docType) doc
            case e of
                Left _ -> return ["Couldn't insert the item."] -- TODO this should be an error
                _ -> return []
            return []
        else do return []
