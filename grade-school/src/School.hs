module School (School, add, empty, grade, sorted) where

import           Data.IntMap.Strict (IntMap, findWithDefault)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (sort)

type School = IntMap [String]

add :: Int -> String -> School -> School
add grade student =
  IntMap.insertWith (\s s' -> sort $ s ++ s') grade [student]

empty :: School
empty = IntMap.empty

grade :: Int -> School -> [String]
grade = findWithDefault []

sorted :: School -> [(Int, [String])]
sorted = IntMap.toList
