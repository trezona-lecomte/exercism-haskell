module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | otherwise =
    Just $ case compare fSum n of
      LT -> Deficient
      EQ -> Perfect
      GT -> Abundant
  where
    fSum = sum $ filter (\m -> n `mod` m == 0) [1..n-1]
