module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum [ n | n <- [0..limit-1], any (\m -> n `rem` m == 0) factors ]
