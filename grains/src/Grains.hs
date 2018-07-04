module Grains (square, total) where

import           Data.Maybe (fromJust)

square :: Int -> Maybe Int
square target
  | target < 1 || target > 64 = Nothing
  | otherwise = (Just . last . take target . iterate (*2)) 1

total :: Int
total = sum $ map (fromJust . square) [1..64]
