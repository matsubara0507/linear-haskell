{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Linear.Sort where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import Prelude.Linear hiding (partition)

heapSort :: [Int] -> [Int]
heapSort xs = unur $ Array.fromList xs $ Array.toList . arrHeapSort

arrHeapSort :: Array Int %1 -> Array Int
arrHeapSort arr = 
  Array.size arr & \(Ur len, arr') -> go (len - 1) (initHeap arr')
  where
    go :: Int -> Array Int %1 -> Array Int
    go n arr'
      | n < 0 = arr'
      | otherwise = go (n - 1) $ maxHeap 0 (n - 1) (swap arr' 0 n)

initHeap :: Array Int %1 -> Array Int
initHeap arr = 
  Array.size arr & \(Ur len, arr') -> go (len - 1) ((len - 1) `div` 2) arr'
  where
    go :: Int -> Int -> Array Int %1 -> Array Int
    go bottom n arr'
      | n < 0 = arr'
      | otherwise = go bottom (n - 1) $ maxHeap n bottom arr'

maxHeap :: Int -> Int -> Array Int %1 -> Array Int
maxHeap root bottom arr = 
  maxIndex arr left right bottom & 
    \(Ur child, arr') -> maxIndex arr' root child bottom & 
      \(Ur root', arr'') -> 
        if root' == root then
          arr''
        else
          maxHeap child bottom (swap arr'' root' root)
  where
    left  = (root * 2) + 1;
    right = (root * 2) + 2;

maxIndex :: Array Int %1 -> Int -> Int -> Int ->  (Ur Int, Array Int)
maxIndex arr i j bottom
  | j > bottom = (Ur i, arr)
  | otherwise = 
      Array.read arr i
        & \(Ur ival, arr') ->
          Array.read arr' j
            & \(Ur jval, arr'') -> (Ur (if ival >= jval then i else j), arr'')


swap :: Array Int %1 -> Int -> Int -> Array Int
swap arr i j =
  Array.read arr i
    & \(Ur ival, arr') ->
      Array.read arr' j
        & \(Ur jval, arr'') -> (Array.set i jval . Array.set j ival) arr''

quickSort :: [Int] -> [Int]
quickSort xs = unur $ Array.fromList xs $ Array.toList . arrQuicksort

arrQuicksort :: Array Int %1 -> Array Int
arrQuicksort arr =
  Array.size arr
    & \(Ur len, arr1) -> go 0 (len - 1) arr1
  where
    go :: Int -> Int -> Array Int %1 -> Array Int
    go lo hi arr
      | lo >= hi = arr
      | otherwise =
          Array.read arr lo
            & \(Ur pivot, arr1) ->
              partition arr1 pivot lo hi
                & \(arr2, Ur ix) ->
                  swap arr2 lo ix
                    & \arr3 ->
                      go lo (ix - 1) arr3
                        & \arr4 -> go (ix + 1) hi arr4

-- | @partition arr pivot lo hi = (arr', Ur ix)@ such that
-- @arr'[i] <= pivot@ for @lo <= i <= ix@,
-- @arr'[j] > pivot@ for @ix < j <= hi@,
-- @arr'[k] = arr[k]@ for @k < lo@ and @k > hi@, and
-- @arr'@ is a permutation of @arr@.
partition :: Array Int %1 -> Int -> Int -> Int -> (Array Int, Ur Int)
partition arr pivot lx rx
  | (rx < lx) = (arr, Ur (lx - 1))
  | otherwise =
      Array.read arr lx
        & \(Ur lVal, arr1) ->
          Array.read arr1 rx
            & \(Ur rVal, arr2) -> case (lVal <= pivot, pivot < rVal) of
              (True, True) -> partition arr2 pivot (lx + 1) (rx - 1)
              (True, False) -> partition arr2 pivot (lx + 1) rx
              (False, True) -> partition arr2 pivot lx (rx - 1)
              (False, False) ->
                swap arr2 lx rx
                  & \arr3 -> partition arr3 pivot (lx + 1) (rx - 1)
