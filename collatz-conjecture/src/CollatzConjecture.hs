module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatzWithSteps n 0

collatzWithSteps :: Integer -> Integer -> Maybe Integer
collatzWithSteps n steps
  | n < 1 = Nothing
  | n == 1 = Just steps
  | otherwise =
    if even n then
      collatzWithSteps (n `quot` 2) (steps+1)
    else
      collatzWithSteps (n * 3 + 1) (steps+1)
