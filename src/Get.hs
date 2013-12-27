{-# LANGUAGE OverloadedStrings #-}

module Get (
    get
  , constructSelection
  , getFlashcards
  , getGoals
  , recordGet
  , getLastGet
  , getLastQueryForOne
  , getDocs
  , runLastGet
) where

import Database.MongoDB hiding (group, sort)
import Data.Time
import Data.Time.Format.Human
import Control.Monad.Trans (liftIO)
import Data.List hiding (find, insert)
import Data.Text (unpack, Text)
import Data.Int
import Data.Char
import Data.Monoid
import Data.Function (on)
import qualified Data.Map as M

import Utils

isPriority :: String -> Bool
isPriority w = case w of
  ('p':restOfWord) -> isInteger restOfWord
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
  case (look "tags" doc) of
    Left _ -> ""
    Right tags -> let Array ts = tags
                      tgs = [unpack tag | String tag <- ts]
                  in showTagsList "" tgs

displayTags :: [Document] -> [String]
displayTags docs = map displayTag docs

getFormattedDocs :: DocType -> UTCTime -> [Document] -> [String] -> [String] -> [String]
getFormattedDocs docType currentTime docs args resultsSoFar = case docs of
  [] -> []
  _ -> case resultsSoFar of -- the first time looping through, just
                            -- create the numbers
    [] -> getFormattedDocs docType currentTime docs args formattedNumbers
            where 
              formattedNumbers = map (\n -> show n ++ " - ") (take (length 
                docs) [1..])
    _ -> case args of
      [] -> case docType of
        Flashcard -> let textQuestions = [question | String question <- map (valueAt
                           (fieldToText Question)) docs]
                         textAnswers = [answer | String answer <- map (valueAt 
                           (fieldToText Answer)) docs]
                         questions = [unpack t | t <- textQuestions]
                         answers = [unpack t | t <- textAnswers]
                         qs = zipWith (++) questions (take (length questions) (repeat "? "))
                         qas = zipWith (++) qs answers
                     in zipWith (++) resultsSoFar qas
        _ -> let texts = [text | String text <- map (valueAt (fieldToText 
                   TextField)) docs]
                 items = [unpack str | str <- texts]
             in zipWith (++) resultsSoFar items
      firstArg:tailArgs -> 
        case firstArg of
          "created" -> 
             let bareDates = map (humanReadableTime' currentTime) [itemDate | 
                   UTC itemDate <- (map (valueAt (fieldToText Created)) docs)]
                 dates = zipWith (++) bareDates (take (length bareDates) (repeat
                   " - "))
                 results = zipWith (++) resultsSoFar dates
             in getFormattedDocs docType currentTime docs tailArgs results
          "with" -> 
             case (head tailArgs) of 
              "tags" -> getFormattedDocs docType currentTime docs (tail tailArgs) $
                zipWith (++) resultsSoFar (displayTags docs)
          _ -> getFormattedDocs docType currentTime docs tailArgs resultsSoFar

runLastGet :: DatabaseName -> IO [String]
runLastGet dbName = do
  lastGet <- getLastGet dbName
  get dbName (words lastGet)

getLastGet :: DatabaseName -> IO String
getLastGet dbName = do
  pipe <- sharedPipe
  let query = select [] (docTypeToText LastGet)
  mdoc <- run pipe dbName $ findOne query
  case mdoc of
    Left failure -> do putStrLn $ show failure
                       return ""
    Right mDoc -> case mDoc of
      Just doc -> do let String result = valueAt (fieldToText TextField) doc
                     return $ unpack result

getLastQueryForOne :: DatabaseName -> Int -> IO Query
getLastQueryForOne dbName n = do
  pipe <- sharedPipe
  lastGet <- getLastGet dbName
  let docType = getDocType $ head (words lastGet)
  docs <- getDocs dbName $ words lastGet
  let ObjId itemId = valueAt (fieldToText ItemId) (docs !! (n-1))
      query = select [(fieldToText ItemId) =: itemId] (docTypeToText docType)
  return query

recordGet :: DatabaseName -> String -> IO ()
recordGet dbName input = do
  pipe <- sharedPipe
  let query = select [] (docTypeToText LastGet)
  mdoc <- run pipe dbName $ findOne query
  case mdoc of
    Left failure -> putStrLn $ show failure
    Right mDoc -> case mDoc of
      Nothing -> do let newDoc = [(fieldToText TextField) =: input]
                    run pipe dbName $ insert (docTypeToText LastGet) newDoc
                    return ()
      Just _ -> do let selection = select [] (docTypeToText LastGet)
                       modifier = ["$set" =: [(fieldToText TextField) =: input]]
                   run pipe dbName $ modify selection modifier
                   return ()

getDocs :: DatabaseName -> [String] -> IO [Document]
getDocs dbName args = do
  pipe <- sharedPipe
  let (query, keywords, docType) = getQueryAndKeywords args
  case keywords of 
    [] -> do
      cursor <- run pipe dbName $ find query
      mDocs <- run pipe dbName $ rest (case cursor of Right c -> c)
      case mDocs of
        Right docs -> return docs
    _ -> do
      actionResult <- ensureIndexForTextSearch dbName docType
      case actionResult of
        Left failure -> do putStrLn $ show failure
                           return []
        Right () -> do
          mDoc <- run pipe dbName $ runCommand $ 
            getTextSearchArgument docType keywords query
          case mDoc of
            Left failure -> do putStrLn $ show failure
                               return []
            Right doc -> let Array results = valueAt "results" doc
                             ds = [d | Doc d <- results]
                         in return ds

get :: DatabaseName -> [String] -> IO [String]
get dbName arguments = do
  let args = case arguments of
        "done":tailArgs -> ["todo"] ++ ["done"] ++ tailArgs
        a -> a
  case args of
    docTypeArg:tailArgs -> case tailArgs of 
      "tags":[] -> do
        recordGet dbName (unwords args)
        getTags dbName (getDocType docTypeArg)
      _ -> do
        recordGet dbName (unwords args)
        docs <- getDocs dbName args
        case docs of 
          [] -> return []
          _ -> do
            currentTime <- getCurrentTime
            return $ getFormattedDocs (getDocType docTypeArg) currentTime docs args []

frequencyList :: [String] -> [(String, Int)]
frequencyList s = map (\l->(head l, length l)) . group . sort $ s

pairToString :: (String, Int) -> String
pairToString p = case p of
  (s, i) -> (show i) ++ " " ++ s

getTags :: DatabaseName -> DocType -> IO [String]
getTags dbName docType = do
  let query = select [] (docTypeToText docType)
  pipe <- sharedPipe
  cursor <- run pipe dbName $ find query
  mDocs <- run pipe dbName $ rest (case cursor of Right c -> c)
  case mDocs of 
    Left _ -> return []
    Right docs ->
      -- mconcat [Just ["hey", "tag"], Just ["yo"], Nothing]
      -- returns Just ["hey","tag","yo"]
      let mTags = mconcat (map (maybeList . (look (fieldToText Tags))) docs)
      in case mTags of 
        Nothing -> return []
        Just tags -> do
          let vl = valueListToStringList tags
              fList = frequencyList vl
              sorted = reverse $ sortBy (compare `on` snd) fList
          return $ map pairToString sorted
  
maybeList :: Maybe Value -> Maybe [Value]
maybeList mv =
  case mv of 
    Nothing -> Nothing
    Just v -> do 
      let Array av = v
      return av

valueListToStringList :: [Value] -> [String]
valueListToStringList vl = map unpack [s | String s <- vl]

getQueryAndKeywords :: [String] -> (Query, [String], DocType)
getQueryAndKeywords arguments = case arguments of
  docTypeArg:args -> 
    let (arguments, ks) = break (isUpper . head) args
        query = constructSelection (getDocType docTypeArg) arguments [] 
          [(fieldToText Done) =: ["$exists" =: False]]
    in (query, ks, getDocType docTypeArg)

getTextSearchArgument :: DocType -> [String] -> Query -> Document
getTextSearchArgument docType keywords query =
  ["text" =: (docTypeToText docType),
    "search" =: (unwords keywords), 
      "filter" =: (selector $ selection query)]

ensureIndexForTextSearch :: DatabaseName -> DocType -> IO (Either Failure ())
ensureIndexForTextSearch dbName docType = do
  let order = [(fieldToText TextField) =: (1 :: Int32)]
      docIndex =  index (docTypeToText docType) order
  pipe <- sharedPipe
  run pipe dbName $ createIndex docIndex

getGoals :: DatabaseName -> IO [Document]
getGoals dbName = do
  pipe <- liftIO sharedPipe
  let selection = select [] (docTypeToText Goal)
  cursor <- run pipe dbName $ find selection
  mDocs <- run pipe dbName $ rest (case cursor of Right c -> c)
  case mDocs of 
    Left _ -> return []
    Right docs -> return docs

getFlashcards :: DatabaseName -> [String] -> IO [Document]
getFlashcards dbName args = do
  pipe <- liftIO sharedPipe
  let selection = select [(fieldToText Tags) =: ["$all" =: args]]
        (docTypeToText Flashcard)
  cursor <- run pipe dbName $ find selection
  mdocs <- run pipe dbName $ rest (case cursor of Right c -> c)
  case mdocs of 
    Left _ -> return []
    Right docs -> return docs
                            
-- | Recursive function that builds up the selector based on args
-- When there are no args left to examine, we check if we've
-- recursively accumulated a list of tags
constructSelection :: DocType -> [String] -> [String] -> Selector -> 
  Query
constructSelection docType args tagsSoFar selector =
  case args of
    "by":tailArgs -> 
      select [(fieldToText DueBy) =: ["$gt" =:
        beginningOfTime, "$lte" =: readDate (head tailArgs)]] 
          (docTypeToText docType)
    "done":tailArgs -> -- does not merge with the current selector. 
                       -- must replace the selection for Done does not exist
      constructSelection docType tailArgs tagsSoFar 
        [(fieldToText Done) =: ["$exists" =: True]]
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
                   ["$all" =: tagsSoFar]]
             in select (merge selector tagsSelector) (docTypeToText docType)
