{-# LANGUAGE OverloadedStrings #-}

module Migrate (
    addDateCreatedToAllCollections
) where

import Database.MongoDB
import Data.Time
import Control.Monad.Trans

import Utils

addDateCreatedToAllCollections :: IO Pipe -> DatabaseName -> 
  IO [Either Failure ()]
addDateCreatedToAllCollections sharedPipe dbName = do
  pipe <- liftIO $ sharedPipe
  cols <- liftIO $ run pipe dbName allCollections
  let collections = case cols of
                          Left x -> []
                          Right c -> c
  mapM (addDateCreatedToAllItems pipe dbName) collections

addDateCreatedToAllItems :: Pipe -> DatabaseName -> Collection -> 
  IO (Either Failure ())
addDateCreatedToAllItems pipe dbName collection = do 
  let beginning = read "2013-12-21 00:00:00" :: UTCTime
      selection = ["created" =: ["$exists" =: False]]
      modifier = ["$set" =: [(labelStr Created) =: beginning]]
  run pipe dbName $ modify (select selection collection) modifier
