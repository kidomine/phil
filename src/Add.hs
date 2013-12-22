module Add (
      add
    , getFieldsForTodo
    , tagIsNew
) where

import Database.MongoDB
import Data.Text (pack)
import Data.Time
import Data.Char
import Data.Int

import Utils
import Validate

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
            | (firstLetter:tailLetters) == "by" ->
                getFieldsForTodo (merge doc 
                    [(fieldToText DueBy) =: (readDate (head tailWords))])
                        (tail tailWords)
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
    if new then do
        now <- getCurrentTime
        e <- run pipe dbName $ insert (docTypeToText Tag) $
                [(fieldToText TextField) =: pack t,
                 (fieldToText TypeField) =: 
                    (docTypeToText docType),
                 (fieldToText Created) =: now]
        return ()
    else
        return ()

add' :: IO Pipe -> DatabaseName -> DocType -> [String] -> IO [String]
add' sharedPipe dbName docType inputWords = do
    pipe <- sharedPipe
    if docIsValid docType inputWords
        then do
            mapM (addNewTag pipe dbName docType) 
                (takeWhile (\wrd -> not $ isUpper (head wrd)) inputWords)
            doc <- getFieldsForType docType inputWords
            e <- run pipe dbName $ insert (docTypeToText docType) doc
            case e of
                Left _ -> return ["Couldn't insert the note."]
                _ -> return []
            return []
        else do return []


add :: DatabaseName -> DocType -> [String] -> IO ([String])
add = add' sharedPipe

