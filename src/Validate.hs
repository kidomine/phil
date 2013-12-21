module Validate (
      docIsValid
    , noteIsValid
) where

import Data.Char

import Utils

docIsValid :: DocType -> [String] -> Bool
docIsValid docType inputWords = case docType of
    Note -> noteIsValid inputWords
    Todo -> noteIsValid inputWords

noteIsValid :: [String] -> Bool
noteIsValid inputWords = any
    (\word -> isUpper (head word)) inputWords

todoIsValid = noteIsValid
