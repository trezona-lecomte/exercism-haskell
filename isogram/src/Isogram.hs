module Isogram (isIsogram) where

import           Data.Char (isLetter, toUpper)

isIsogram :: String -> Bool
isIsogram = (check . map toUpper) . filter isLetter
  where check "" = True
        check (l:ls)
          | l `elem` ls = False
          | otherwise = isIsogram ls
