{-# LANGUAGE OverloadedStrings #-}

module Edit (
    edit
) where

import System.IO
import System.Process
import Database.MongoDB
import Data.Time.Clock (getCurrentTime)
import Data.Text (unpack)
import Utils

edit :: DatabaseName -> [String] -> IO [String]
edit dbName args = do
  pipe <- sharedPipe
  let docType = getDocType $ head args
      n = read (args !! 1) :: Int
  cursor <- run pipe dbName $ find (select [] (docTypeToText docType)) 
  mDocs <- run pipe dbName $ rest (case cursor of Right c -> c)
  case mDocs of
    Left failure -> return []
    Right docs -> do
      currentTime <- getCurrentTime
      let ObjId itemId = valueAt (fieldToText ItemId) (docs !! (n-1))
          query = select [(fieldToText ItemId) =: itemId]
            (docTypeToText docType)
      mdoc <- run pipe dbName $ findOne query
      case mdoc of
        Left failure -> do putStrLn $ show failure
                           return []
        Right mDoc -> case mDoc of
          Nothing -> return []
          Just doc -> do
            let String text = valueAt (fieldToText TextField) doc
                filename = "/Users/rose/phil/tempedit"
                selection = select [(fieldToText ItemId) =: itemId]
                  (docTypeToText docType)
            writeFile filename (unpack text)
            exitSuccess <- system $ "vi " ++ filename
            modifiedText <- readFile filename
            exitSuccess <- system $ "rm " ++ filename
            let updateTimeModifier = ["$set" =: [(fieldToText Updated) =: 
                  currentTime]]
            run pipe dbName $ modify selection updateTimeModifier
            let updateTextModifier = ["$set" =: [(fieldToText TextField) =:
                  (init modifiedText)]] -- strips the newline
            run pipe dbName $ modify selection updateTextModifier
            return []
