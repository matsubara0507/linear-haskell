{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MonoLocalBinds #-}

module SortedList (SortedList, fromList) where

import Prelude.Linear

-- type OrdL a = (Ord a, Movable a)

-- compareL :: forall a . OrdL a => a %1 -> a %1 -> (Ordering, a, a)
-- compareL a b = go (move a) (move b) 
--   where
--     go :: Ur a %1 -> Ur a %1 -> (Ordering, a, a)
--     go (Ur x) (Ur y) = (compare x y, x, y)

class OrdL a where
  compareL :: a %1 -> a %1 -> (Ordering, a, a)

instance (Ord a, Movable a) => OrdL a where
  compareL a b = go (move a) (move b) 
    where
      go :: Ur a %1 -> Ur a %1 -> (Ordering, a, a)
      go (Ur x) (Ur y) = (compare x y, x, y)

newtype SortedList a = Sorted [a] deriving (Show)

fromList :: forall a. OrdL a => [a] %1 -> SortedList a
fromList [] = Sorted []
fromList [a] = Sorted [a]
fromList xs = go (split xs) 
  where
   go :: ([a], [a]) %1 -> SortedList a
   go (left, right) = merge (fromList left) (fromList right)

split :: [a] %1 -> ([a], [a])
split []      = ([], [])
split [x]     = ([x], [])
split (x:y:z) = go (x, y) (split z) 
  where
    go :: (a, a) %1 -> ([a], [a]) %1 -> ([a], [a])
    go (a, b) (ax, bx) = (a:ax, b:bx)

merge :: forall a. OrdL a  => SortedList a %1 -> SortedList a %1 -> SortedList a
merge (Sorted []) bs = bs
merge as (Sorted []) = as
merge (Sorted (a:as)) (Sorted (b:bs)) = go (compareL a b) (Sorted as) (Sorted bs) 
  where
    go :: (Ordering, a, a) %1 -> SortedList a %1 -> SortedList a %1 -> SortedList a
    go (EQ, k, l) ks ls = k `cons` (l `cons` merge ks ls)
    go (LT, k, l) ks ls = k `cons` merge ks (l `cons` ls)
    go (GT, k, l) ks ls = l `cons` merge (k `cons` ks) ls

    cons :: a %1 -> SortedList a %1 -> SortedList a
    cons x (Sorted xs) = Sorted (x : xs)

-- uncons :: Movable a => SortedList a %1 -> (Maybe (Ur a), SortedList a)
-- uncons (Sorted [])     = (Nothing, Sorted [])
-- uncons (Sorted (x:xs)) = (Just (move x), Sorted xs)

-- merge' :: forall a. OrdL a  => SortedList a %1 -> SortedList a %1 -> SortedList a
-- merge' (Sorted []) bs = bs
-- merge' as (Sorted []) = as
-- merge' as bs =
--   uncons as & \(Just (Ur a), as') -> 
--     uncons bs & \(Just (Ur b), bs') ->
--       case compare a b of
--         EQ -> a `cons` (b `cons` merge' as' bs')
--         LT -> a `cons` merge' as' (b `cons` bs')
--         GT -> b `cons` merge' (a `cons` as') bs'
