module Done (
    completeTodo
) where

import Database.MongoDB
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)

import Utils

completeTodo :: DatabaseName -> Int -> IO [String]
completeTodo dbName n = do
  pipe <- sharedPipe
  cursor <- run pipe dbName $ find (select [] (docTypeToText Todo))
  mDocs <- run pipe dbName $ rest (case cursor of Right c -> c)
  case mDocs of
    Left failure -> return []
    Right docs -> do
      currentTime <- getCurrentTime
      let ObjId itemId = valueAt (fieldToText ItemId) (docs !! (n-1))
          selection = select [(fieldToText ItemId) =: itemId] 
            (docTypeToText Todo)
          modifier = [pack "$set" =: [(fieldToText Done) =: currentTime]]
      run pipe dbName $ modify selection modifier
      return []
