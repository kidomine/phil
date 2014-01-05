{-# LANGUAGE OverloadedStrings #-}

module Edit (
    edit
) where

import System.IO
import System.Process
import Database.MongoDB
import Data.Time
import Data.Text (unpack, pack)
import Data.Int
--import Data.Bson

import Get
import Utils

instance Val Value where
  val   = id
  cast' = Just

edit :: DatabaseName -> Int -> IO [String]
edit dbName n = do
  pipe <- sharedPipe
  query <- getLastQueryForOne dbName n
  mdoc <- run pipe dbName $ findOne query
  currentTime <- getCurrentTime
  case mdoc of
    Left failure -> do putStrLn $ show failure
                       return []
    Right mDoc -> case mDoc of
      Nothing -> return []
      Just doc -> do
        --let String text = valueAt (labelStr TextLabel) doc
        let filename = "/Users/rose/phil/tempedit"
            sel = selection query
            excludedLabels = ["_id", "created", "updated", "questionId", 
                              "goalId", "count", "testCount"]
            modifiableFields = exclude excludedLabels doc
            unmodifiableFields = include excludedLabels doc
        writeFile filename (docToStr modifiableFields)
        exitSuccess <- system $ "vi " ++ filename
        modifiedString <- readFile filename
        putStrLn modifiedString
        let modifiedDoc = merge unmodifiableFields (strToDoc modifiedString)
        mapM_ (updateField dbName sel) modifiedDoc
        exitSuccess <- system $ "rm " ++ filename
        return []

updateField :: DatabaseName -> Selection -> Field -> IO ()
updateField dbName sel f = do
  pipe <- sharedPipe
  let modifier = ["$set" =: [f]]
  run pipe dbName $ modify sel modifier
  return ()

    -- TODO add some error stuff in here

docToStr :: Document -> String
docToStr doc = unlinesByFunnyChar $ zipWith (++) (map fieldToStr doc) 
  (take (length doc) $ repeat "\n")

    -- TODO run a query that gets all fields that a document contains.

-- append field and value to the temp file, starting with a |. 
--
-- read in the file: for every line that starts with a |, parse the field and 
-- value, and run a series of modifiers, like "$set" =: [label =: value]

fieldToStr :: Field -> String
fieldToStr f = (unpack $ label f) ++ ":" ++ (show $ value f)

strToField :: String -> Field
strToField s =
  case splitAboutSubstring s ":" of
    Just (label, valueString) -> (pack label) =: (readValue (pack label) valueString)

readValue :: Label -> String -> Value
readValue label valueString = 
  case label of
    "tags" -> val (read valueString :: [String])
    "text" -> val  (read valueString :: String)
    "type" -> val (read valueString :: String)
    "priority" -> val (read valueString :: Int32)
    "dueBy" -> val (read valueString :: UTCTime)
    "question" -> val (read valueString :: String)
    "answer" -> val (read valueString :: String)
    "count" -> val (read valueString :: Int32)
    "score" -> val (read valueString :: Int32)
    "testCount" -> val (read valueString :: Int)
    "startDate" -> val (read valueString :: UTCTime)
    "endDate" -> val (read valueString :: UTCTime)
    "done" -> val (read valueString :: UTCTime)
    "answerImageFilename" -> val (read valueString :: String)
    "questionImageFilename" -> val (read valueString :: String)
    x -> error $ "Hey Rose, the bad value is " ++ (show x)

strToDoc :: String -> Document
strToDoc s = map strToField (init (linesByFunnyChar s))

unlinesByFunnyChar :: [String] -> String
unlinesByFunnyChar = concatMap (++ "…")

linesByFunnyChar :: String -> [String]
linesByFunnyChar s =  let (l, s') = break (== '…') s
                           in  l : case s' of
                                        []      -> []
                                        (_:s'') -> linesByFunnyChar s''
