module Validate (
    docIsValid
  , noteIsValid
  , flashcardIsValid
  , goalIsValid
) where

import Data.Char

import Utils

docIsValid :: DocType -> [String] -> Bool
docIsValid docType inputWords = case docType of
  Note -> noteIsValid inputWords
  Todo -> todoIsValid inputWords
  Flashcard -> flashcardIsValid inputWords
  Goal -> goalIsValid inputWords

noteIsValid :: [String] -> Bool
noteIsValid inputWords = any
  (\word -> isUpper (head word)) inputWords

todoIsValid = noteIsValid

flashcardIsValid inputWords = noteIsValid inputWords && (any (=='?')
  (unlines inputWords))

goalIsValid = noteIsValid
