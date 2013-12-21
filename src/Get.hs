module Get (
      get
    , constructSelection
    , tagsSelector
) where

import Database.MongoDB
import Data.Time
import Data.Time.Format.Human
import Control.Monad.Trans (liftIO)
import Data.List hiding (find)
import Data.Text (pack, unpack, Text)
import Data.Int

import Utils

isPriority :: String -> Bool
isPriority w = case w of
    ('p':restOfWord) ->
       isInteger restOfWord
    _ -> False

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

get' :: IO Pipe -> DatabaseName -> [String] -> IO [String]
get' sharedPipe dbName arguments = do
    pipe <- liftIO sharedPipe
    case arguments of
        docTypeArg:args -> do
            let docType = getDocType docTypeArg
                selection = constructSelection docType args
            cursor <- run pipe (databaseNameToString dbName)
                $ find selection
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
