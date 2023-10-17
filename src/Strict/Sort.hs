{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Strict.Sort where

import Prelude

import Data.List (unfoldr)

data Tree a = Null | Fork a (Tree a) (Tree a) deriving Show

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty _    = False

minElem :: Tree a -> a
minElem (Fork x _ _) = x
minElem _            = error "minElem"

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork _ a b) = merge a b
deleteMin _            = error "deleteMin"

insert :: Ord a => a -> Tree a -> Tree a
insert x = merge (Fork x Null Null)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
  | minElem a <= minElem b = join a b
  | otherwise              = join b a

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork x a b) c = Fork x b (merge a c)
join _ _ = error "join"

heapSort :: [Int] -> [Int]
heapSort xs = unfoldr takeToList (foldr insert Null xs)
  where
    takeToList heap
      | isEmpty heap = Nothing
      | otherwise    = Just (minElem heap, deleteMin heap)
