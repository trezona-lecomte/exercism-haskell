{-# LANGUAGE FlexibleContexts #-}

module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    , foldr
    ) where

data LinkedList a
  = Nil
  | Next a (LinkedList a)
  deriving (Eq, Show)

instance Foldable LinkedList where
  foldr _ acc Nil         = acc
  foldr f acc (Next x xs) = f x (foldr f acc xs)

datum :: LinkedList a -> a
datum Nil        = error "Partial function, mate."
datum (Next x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr Next nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new x Nil = Next x nil
new x xs  = Next x xs

next :: LinkedList a -> LinkedList a
next Nil         = error "Partial function, chum."
next (Next _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip new) nil

toList :: LinkedList a -> [a]
toList = foldr (:) []
