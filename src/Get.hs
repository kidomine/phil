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

{-
commaSeparatedTags :: String -> [String] -> String
commaSeparatedTags stringSoFar tags = case tags of 
    [] -> "[]"
    ts -> (init ts) ++ (last ts)
    
    
    
    case stringSoFar of
            string ++ ", " -> string
            string -> string
    t:ts -> commaSeparatedTags (stringSoFar ++ ", " ++ t) ts
-}

{-
stringOfTags :: [String] -> String
stringOfTags tags = "[" ++ ( tags) ++ "]"
-}


showTagsList :: String -> [String] -> String
showTagsList listSoFar tagsRemaining =
    case tagsRemaining of
        [] -> case listSoFar of
                "" -> ""
                _ -> listSoFar ++ "] - "
        t:ts -> case listSoFar of 
                    "" -> showTagsList ("[" ++ t) ts
                    _ -> showTagsList (listSoFar ++ ", " ++ t) ts

-- | If a tag exists, return "tag - "
-- otherwise, return ""
displayTag :: Document -> String
displayTag doc = 
    case (look (pack "tags") doc) of
        Left _ -> ""
        Right tags -> let Array ts = tags
                          tgs = [unpack tag | String tag <- ts]
                      in showTagsList "" tgs
--[text | String text <- Array tags]
displayTags :: [Document] -> [String]
displayTags docs = map displayTag docs
{-
    let tags = [tag | String tag <-
            (map (valueAt (fieldToText Tags)) 
                docs)]
        displayedTags = zipWith (++) (map show tags)
            (take (length tags) (repeat " - "))
        results = zipWith (++) resultsSoFar 
            displayedTags
    in getFormattedDocs currentTime docs
        (tail tailArgs) results
        -}
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
                   firstArg:tailArgs -> 
                     case firstArg of
                         "created" -> 
                            let bareDates = map (humanReadableTime' 
                                    currentTime) [itemDate | UTC itemDate <- 
                                        (map (valueAt 
                                            (fieldToText Created)) docs)]
                                dates = zipWith (++) bareDates (take 
                                   (length bareDates) (repeat " - "))
                                results = zipWith (++) resultsSoFar dates
                            in getFormattedDocs currentTime docs 
                                tailArgs results
                         "with" -> 
                            case (head tailArgs) of 
                             "tags" -> 
                                getFormattedDocs currentTime docs 
                                    (tail tailArgs) $ zipWith (++)
                                        resultsSoFar (displayTags docs)
                         _ -> getFormattedDocs currentTime docs tailArgs 
                            resultsSoFar

get' :: IO Pipe -> DatabaseName -> [String] -> IO [String]
get' sharedPipe dbName arguments = do
    pipe <- liftIO sharedPipe
    case arguments of
        docTypeArg:args -> do
            let docType = getDocType docTypeArg
                selection = constructSelection docType args
            cursor <- run pipe dbName $ find selection
            docs <- run pipe dbName $ rest (case cursor of Right c -> c)
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
    ts -> merge [] (selector ++ [(fieldToText Tags) =: [pack "$all" =: ts]])

-- | Recursive function that builds up the selector based on args
-- When there are no args left to examine, we check if we've
-- recursively accumulated a list of tags
constructTodoSelection :: Selector -> [String] -> [String] -> Query
constructTodoSelection selector inputWords tagsSoFar =
    case inputWords of
        [] -> case tagsSoFar of
            [] -> select selector (docTypeToText Todo)
            _ -> select (merge selector $ tagsSelector [] tagsSoFar) 
                (docTypeToText Todo)
        firstWord:tailWords | isPriority firstWord ->
                                case firstWord of
                                    firstLetter:restOfWord ->
                                        constructTodoSelection (merge
                                            selector [(fieldToText Priority) =:
                                                (read restOfWord :: Int32)])
                                                    tailWords tagsSoFar
                            | firstWord == "by" -> select 
                                [(fieldToText DueBy) =: [pack "$gt" =: 
                                    beginningOfTime, pack "$lte" =: readDate 
                                        (head tailWords)]] (docTypeToText Todo)
                            | wordIsReserved firstWord ->
                                constructTodoSelection selector tailWords 
                                    tagsSoFar
                            | otherwise -> let newTags = case tagsSoFar of
                                                [] -> [firstWord]
                                                _ -> tagsSoFar ++ [firstWord]
                                           in constructTodoSelection
                                              selector tailWords newTags

constructNoteSelection :: Selector -> [String] -> [String] -> Query
constructNoteSelection selector inputWords tagsSoFar =
    case inputWords of
        [] -> case tagsSoFar of
            [] -> select selector (docTypeToText Note)
            _ -> select (merge selector $ tagsSelector [] tagsSoFar) 
                (docTypeToText Note)
        firstWord:tailWords | wordIsReserved firstWord ->
                                 constructNoteSelection
                                    selector tailWords tagsSoFar
                            | otherwise -> let newTags = case tagsSoFar of
                                                           [] -> [firstWord]
                                                           _ -> tagsSoFar ++ 
                                                               [firstWord]
                                           in constructNoteSelection selector
                                                 tailWords newTags

constructSelection :: DocType -> [String] -> Query
constructSelection docType args =
    let constructSelection = case docType of
                                Todo -> constructTodoSelection [] args []
                                Note -> constructNoteSelection [] args []
    in case args of
        firstArg:tailArgs | firstArg == "tags" -> 
                                select [(fieldToText TypeField) =: 
                                    (docTypeToText docType)] (docTypeToText Tag)
                          | firstArg == "with" -> 
                                select [] (docTypeToText docType)
                          | otherwise -> constructSelection
        _ -> constructSelection
