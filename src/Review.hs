{-# LANGUAGE OverloadedStrings #-}

module Review (
    review
) where

import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Data.Text (unpack)

import Utils

constructFlashcardSelection :: Selector -> [String] -> [String] -> Query
constructFlashcardSelection selector inputWords tagsSoFar =
  case inputWords of
    [] -> case tagsSoFar of
      _ -> let newSelector = [(fieldToText Tags) =: ["$all" =: tagsSoFar]]
               in select (merge selector newSelector) (docTypeToText Flashcard)
    firstWord:tailWords 
      | wordIsReserved firstWord ->
           constructFlashcardSelection
              selector tailWords tagsSoFar
      | otherwise -> let newTags = case tagsSoFar of
                                     [] -> [firstWord]
                                     _ -> tagsSoFar ++ [firstWord]
                     in constructFlashcardSelection selector tailWords newTags

getQA :: Document -> String
getQA doc = let String question = valueAt (fieldToText Question) doc
                String answer = valueAt (fieldToText Answer) doc
            in (unpack question) ++ "?\n    " ++ (unpack answer)

getFormattedFlashcards :: [Document] -> [String]
getFormattedFlashcards docs =
  let fs = map getQA docs
  in zipWith (++) fs (take (length fs) (repeat "\n\n"))

review :: DatabaseName -> [String] -> IO String
review dbName inputWords = do
  pipe <- liftIO sharedPipe
  let selection = constructFlashcardSelection
  cursor <- run pipe dbName $ find $ selection [] inputWords []
  docs <- run pipe dbName $ rest (case cursor of Right c -> c)
  case docs of
    Left _ -> return []
    Right documents -> return $ concat $ getFormattedFlashcards documents
