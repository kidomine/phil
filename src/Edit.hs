{-# LANGUAGE OverloadedStrings #-}

module Edit (
    edit
) where

import System.IO
import System.Process
import Database.MongoDB
import Data.Time.Clock (getCurrentTime)
import Data.Text (unpack)

import Get
import Utils

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
        let String text = valueAt (fieldToText TextField) doc
            filename = "/Users/rose/phil/tempedit"
            sel = selection query
        writeFile filename (unpack text)
        exitSuccess <- system $ "vi " ++ filename
        modifiedText <- readFile filename
        exitSuccess <- system $ "rm " ++ filename
        let updateTimeModifier = ["$set" =: [(fieldToText Updated) =: 
              currentTime]]
        run pipe dbName $ modify sel updateTimeModifier
        let updateTextModifier = ["$set" =: [(fieldToText TextField) =:
              (init modifiedText)]] -- strips the newline
        run pipe dbName $ modify sel updateTextModifier
        return []
