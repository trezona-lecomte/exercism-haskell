module Acronym (abbreviate) where

import           Data.Char       (toUpper)
import           Data.List       (nub)
import           Text.Regex.TDFA ((=~))


abbreviate :: String -> String
abbreviate input = map toUpper $ concat uniqueMatches
  where
    uniqueMatches = filter isSingle $ concatMap nub (input =~ startOfWord :: [[String]])
    isSingle s = length s == 1

startOfWord :: String
startOfWord = "(\\b[A-z])([a-z]+([A-Z]{1}))*"
