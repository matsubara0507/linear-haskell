{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Linear.Sort where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import GHC.Stack
import Prelude.Linear

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


swap :: (HasCallStack) => Array Int %1 -> Int -> Int -> Array Int
swap arr i j =
  Array.read arr i
    & \(Ur ival, arr') ->
      Array.read arr' j
        & \(Ur jval, arr'') -> (Array.set i jval . Array.set j ival) arr''