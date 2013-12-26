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
  lastGet <- getLastGet dbName
  let docType = getDocType $ head (words lastGet)
  docs <- getDocs dbName $ words lastGet
  let ObjId itemId = valueAt (fieldToText ItemId) (docs !! (n-1))
      query = select [(fieldToText ItemId) =: itemId] (docTypeToText docType)
  run pipe dbName $ deleteOne query 
  return ()

-- | Delete all documents of a certain type from db
deleteAll :: DatabaseName -> DocType -> IO [String]
deleteAll dbName docType = do
  pipe <- sharedPipe
  run pipe dbName $ delete (select [] (docTypeToText docType))
  return []
