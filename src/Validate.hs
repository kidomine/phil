module Validate (
      docIsValid
    , noteIsValid
    , flashcardIsValid
    , eventIsValid
    , goalIsValid
) where

import Data.Char

import Utils

docIsValid :: DocType -> [String] -> Bool
docIsValid docType inputWords = case docType of
    Note -> noteIsValid inputWords
    Todo -> todoIsValid inputWords
    Flashcard -> flashcardIsValid inputWords
    Event -> eventIsValid inputWords
    Goal -> goalIsValid inputWords

noteIsValid :: [String] -> Bool
noteIsValid inputWords = any
    (\word -> isUpper (head word)) inputWords

todoIsValid = noteIsValid

flashcardIsValid inputWords = noteIsValid inputWords && (any (=='?')
    (unlines inputWords))

eventIsValid :: [String] -> Bool
eventIsValid inputWords = noteIsValid inputWords && 
  case (splitDateTimeRangeTagsAndText $ unwords inputWords) of
    Nothing -> False
    Just _ -> True

goalIsValid = noteIsValid
