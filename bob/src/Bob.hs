{-# LANGUAGE FlexibleContexts #-}

module Bob (responseFor) where

import           Text.Regex.TDFA ((=~))


responseFor :: String -> String
responseFor q
  | q =~ nothing = "Fine. Be that way!"
  | q =~ yelledQuestion = "Calm down, I know what I'm doing!"
  | q =~ yell && q =~ alphas = "Whoa, chill out!"
  | q =~ calmQuestion = "Sure."
  | otherwise = "Whatever."
  where
    nothing = "^(\\W)+$"
    yelledQuestion = "[A-Z\\s]+\\?+"
    yell = "^[^a-b]+[!]*$"
    alphas = ".*[A-z].*"
    calmQuestion = "[0-9A-z\\s, :)]+\\?+$"
