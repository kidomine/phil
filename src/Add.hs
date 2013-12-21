module Add (
      add
    , getFieldsForTodo
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

add' :: IO Pipe -> DatabaseName -> DocType -> [String] -> IO [String]
add' sharedPipe dbName docType inputWords = do
    pipe <- sharedPipe
    if docIsValid docType inputWords
        then do
            doc <- getFieldsForType docType inputWords
            e <- access pipe master (pack $
                databaseNameToString dbName)
                    $ insert (docTypeToText docType) doc
            case e of
                Left _ -> return ["Couldn't insert the note."]
                _ -> return []
            return []
        else do return []


add :: DatabaseName -> DocType -> [String] -> IO ([String])
add = add' sharedPipe

