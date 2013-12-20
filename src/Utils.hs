module Utils where

import Data.Char

isInteger :: String -> Bool
isInteger st
    | length st == 0 = False
    | length st == 1 = isNumber $ head st
    | otherwise = case st of
        firstChar:tailChars ->
            if (isNumber firstChar) == True
            then (isInteger tailChars) else False
