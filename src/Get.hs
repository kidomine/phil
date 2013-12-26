module Get (
      get
    , constructSelection
    , getFlashcards
    , getGoals
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
             [] -> return []
             docs -> do
                  --putStrLn $ "docs are " ++ (show docs)
                  currentTime <- getCurrentTime
                  return $ getFormattedDocs currentTime docs args []

getGoals :: DatabaseName -> IO [Document]
getGoals dbName = do
  pipe <- liftIO sharedPipe
  let selection = select [] (docTypeToText Goal)
  cursor <- run pipe dbName $ find selection
  mdocs <- run pipe dbName $ rest (case cursor of Right c -> c)
  case mdocs of 
    Left _ -> return []
    Right docs -> return docs

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

getDocs :: DatabaseName -> DocType -> [String] -> IO [Document]
getDocs dbName docType args = do
  pipe <- sharedPipe
  case args of
    "tags":tailArgs -> do
       let selection = select [(fieldToText TypeField) =: 
             (docTypeToText docType)] (docTypeToText Tag)
       cursor <- run pipe dbName $ find selection
       mDocs <- run pipe dbName $ rest (case cursor of Right c -> c)
       case mDocs of 
         Left failure -> do putStrLn $ show failure
                            return []
         Right docs -> return docs
    _ -> let (arguments, ks) = break (isUpper . head) args
             query = constructSelection docType arguments [] []
         in case ks of 
            [] -> do
              cursor <- run pipe dbName $ find query
              mDocs <- run pipe dbName $ rest (case cursor of Right c -> c)
              case mDocs of
                Left failure -> do putStrLn $ show failure
                                   return []
                Right docs -> return docs
            keywords -> do 
              let order = [(fieldToText TextField) =: (1 :: Int32)]
              let i = index (docTypeToText docType) order
              putStrLn $ "index is " ++ show i
              run pipe dbName $ ensureIndex i
              mDoc <- run pipe dbName $ runCommand 
                [pack "text" =: (docTypeToText docType),
                  pack "search" =: (pack $ unwords keywords), 
                    pack "filter" =: (selector $ selection query)]
              case mDoc of
                Left failure -> do putStrLn $ show failure
                                   return []
                Right doc -> let Array results = valueAt (pack "results") doc
                                 ds = [d | Doc d <- results]
                             in return ds
                            
-- | Recursive function that builds up the selector based on args
-- When there are no args left to examine, we check if we've
-- recursively accumulated a list of tags
constructSelection :: DocType -> [String] -> [String] -> Selector -> 
  Query
constructSelection docType args tagsSoFar selector =
  case args of
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
        _ -> let tagsSelector = [(fieldToText Tags) =: 
                   [pack "$all" =: tagsSoFar]]
             in select (merge selector tagsSelector) (docTypeToText docType)
