module Done (
    completeTodo
) where

import Database.MongoDB
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)

import Get
import Utils

completeTodo :: DatabaseName -> Int -> IO [String]
completeTodo dbName n = do
  pipe <- sharedPipe
  query <- getLastQueryForOne dbName n
  currentTime <- getCurrentTime
  let modifier = [pack "$set" =: [(labelStr Done) =: currentTime]]
  run pipe dbName $ modify (selection query) modifier
  return []
