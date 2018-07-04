module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = traverse rnaComp

rnaComp :: Char -> Maybe Char
rnaComp 'G' = Just 'C'
rnaComp 'C' = Just 'G'
rnaComp 'T' = Just 'A'
rnaComp 'A' = Just 'U'
rnaComp  _  = Nothing
