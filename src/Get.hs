module Get (
      get
    , constructSelection
    , getFlashcards
) where

import Database.MongoDB
import Data.Time
import Data.Time.Format.Human
import Control.Monad.Trans (liftIO)
import Data.List hiding (find)
import Data.Text (pack, unpack, Text)
import Data.Int
import Data.Char

import Utils

isPriority :: String -> Bool
isPriority w = case w of
    ('p':restOfWord) ->
       isInteger restOfWord
    _ -> False

--
-- Preparing items for display
--

showTagsList :: String -> [String] -> String
showTagsList listSoFar tagsRemaining =
    case tagsRemaining of
        [] -> case listSoFar of
                "" -> ""
                _ -> listSoFar ++ "] - "
        t:ts -> case listSoFar of 
                    "" -> showTagsList ("[" ++ t) ts
                    _ -> showTagsList (listSoFar ++ ", " ++ t) ts

displayTag :: Document -> String
displayTag doc = 
    case (look (pack "tags") doc) of
        Left _ -> ""
        Right tags -> let Array ts = tags
                          tgs = [unpack tag | String tag <- ts]
                      in showTagsList "" tgs

displayTags :: [Document] -> [String]
displayTags docs = map displayTag docs

getFormattedDocs :: UTCTime -> [Document] -> [String] -> [String] -> [String]
getFormattedDocs currentTime docs args resultsSoFar = case docs of
    [] -> []
    _ -> case resultsSoFar of -- the first time looping through, just
                              -- create the numbers
            [] -> getFormattedDocs currentTime docs args formattedNumbers
                    where 
                      formattedNumbers = map (\n -> show n ++ " - ")
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

get :: DatabaseName -> [String] -> IO [String]
get dbName arguments = do
    case arguments of
      docTypeArg:args -> do
         let docType = getDocType docTypeArg
         mDocs <- getDocs dbName docType args
         case mDocs of
             Left _ -> return []
             Right docs -> do
                  --putStrLn $ "docs are " ++ (show docs)
                  currentTime <- getCurrentTime
                  return $ getFormattedDocs currentTime docs args []

getFlashcards :: DatabaseName -> [String] -> IO [Document]
getFlashcards dbName args = do
    pipe <- liftIO sharedPipe
    let selection = select [(fieldToText Tags) =: [pack "$all" =: args]]
          (docTypeToText Flashcard)
    cursor <- run pipe dbName $ find selection
    mdocs <- run pipe dbName $ rest (case cursor of Right c -> c)
    case mdocs of 
        Left _ -> return []
        Right docs -> return docs

getDocs :: DatabaseName -> DocType -> [String] -> IO (Either Failure [Document])
getDocs dbName docType args = do
  pipe <- sharedPipe
  case args of
    "tags":tailArgs -> do
       let selection = select [(fieldToText TypeField) =: 
             (docTypeToText docType)] (docTypeToText Tag)
       cursor <- run pipe dbName $ find selection
       run pipe dbName $ rest (case cursor of Right c -> c)
       {-
    "with":"tags":tailArgs -> do
       let selection = select [] (docTypeToText Tag)
       cursor <- run pipe dbName $ find selection
       results <- run pipe dbName $ rest (case cursor of Right c -> c)
       putStrLn $ "results for the with query are " ++ (show results)
       return results
       -}
    _ -> let (arguments, keywords) = break (isUpper . head) args
             selection = constructSelection docType arguments [] []
         in case keywords of 
              [] -> do
                --putStrLn $ "selection is " ++ (show selection)
                cursor <- run pipe dbName $ find selection
                run pipe dbName $ rest (case cursor of Right c -> c)
              ks -> do 
                undefined
              {-
                mDoc <- run pipe dbName $ runCommand [pack "text" =:
                  [pack "search" =: (pack $ unwords ks)], 
                    [pack "filter" =: (selector selection)]]
                case mDoc of
                  Left _ -> error
                  Right doc -> return $ Right $ valueAt (pack "results") doc
                  -}
                            
-- | Recursive function that builds up the selector based on args
-- When there are no args left to examine, we check if we've
-- recursively accumulated a list of tags
constructSelection :: DocType -> [String] -> [String] -> Selector -> 
  Query
constructSelection docType args tagsSoFar selector =
  case args of
  {-
    "tags":tailArgs -> do
      select [(fieldToText TypeField) =: (docTypeToText docType)] 
        (docTypeToText Tag)
    "with":tailArgs -> select [] (docTypeToText docType)
    -}
    "by":tailArgs -> 
      select [(fieldToText DueBy) =: [pack "$gt" =:
        beginningOfTime, pack "$lte" =: readDate (head tailArgs)]] 
          (docTypeToText docType)
    firstArg:tailArgs 
      | isPriority firstArg ->
          case firstArg of
            firstLetter:restOfWord -> constructSelection docType
              tailArgs tagsSoFar (merge selector [(fieldToText Priority) 
                =: (read restOfWord :: Int32)])
      | wordIsReserved firstArg ->
          constructSelection docType tailArgs tagsSoFar selector 
      | otherwise -> 
          constructSelection docType tailArgs (tagsSoFar ++ [firstArg])
            selector
    [] -> case tagsSoFar of 
        [] -> select selector (docTypeToText docType)
        _ -> let tagsSelector = [(fieldToText Tags) =: [pack "$all" =: tagsSoFar]]
             in select (merge selector tagsSelector) (docTypeToText docType)
