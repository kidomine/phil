{-# LANGUAGE OverloadedStrings #-}

module Delete (
    deleteItem
  , deleteAll 
) where

import Data.List hiding (find, delete)
import Data.Text (unpack)
import Database.MongoDB

import Utils
import Get

deleteItem :: DatabaseName -> Int -> IO ()
deleteItem dbName n = do
  pipe <- sharedPipe
  query <- getLastQueryForOne dbName n
  run pipe dbName $ deleteOne $ selection query 
  return ()

-- | Delete all documents of a certain type from db
deleteAll :: DocType -> IO [String]
deleteAll docType = do
  pipe <- sharedPipe
  run pipe TestDB $ delete (select [] (docTypeToText docType))
  return []
