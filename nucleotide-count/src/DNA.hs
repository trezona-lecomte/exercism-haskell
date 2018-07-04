module DNA (nucleotideCounts) where

import           Data.List (nub)
import           Data.Map  (Map, fromList, insertWith, keys)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  if all (`elem` nucleotides) (keys map) then
    Right map
  else
    Left "Invalid strand, mate."
  where
    map = foldl (\m x -> insertWith (+) x 1 m) init xs
    init = fromList $ zip nucleotides $ repeat 0
    nucleotides = ['A', 'C', 'G', 'T']
