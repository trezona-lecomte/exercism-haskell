module Pangram (isPangram) where

import           Data.Char (toLower)

isPangram :: String -> Bool
isPangram []    = False
isPangram input = all (`elem` map toLower input) ['a'..'z']
