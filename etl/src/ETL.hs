module ETL (transform) where

import           Data.Char (toLower)
import qualified Data.List as List
import           Data.Map  (Map, empty, foldrWithKey, insert)

transform :: Map a String -> Map Char a
transform = foldrWithKey mapByChar empty
  where mapByChar n ls m = List.foldr insertAsLower m ls
          where insertAsLower l = insert (toLower l) n
