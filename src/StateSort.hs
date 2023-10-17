{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}

module StateSort where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import Prelude.Linear
import Control.Functor.Linear as Linear

type SArray a = State (Array a) -- 本当は a に Ord を付けたいが割愛

swap :: Int -> Int -> SArray a ()
swap i j = Linear.do
  Ur ival <- state (Array.get i)
  Ur jval <- state (Array.get j)
  modify (Array.set i jval . Array.set j ival)

downto :: Int -> Int -> (Int -> SArray a ()) -> SArray a ()
downto start end f = go start
  where
    go n
      | n < end   = return ()
      | otherwise = f n >> go (n - 1)

sortWith :: SArray Int () -> [Int] -> [Int]
sortWith f xs = unur $ Array.fromList xs (Array.toList . execState f)

heapSort :: [Int] -> [Int]
heapSort = sortWith $ Linear.do
  Ur len <- state Array.size
  downto ((len - 1) `div` 2) 0 $ \n ->
    maxHeap n (len - 1)
  downto (len - 1) 1 $ \n -> Linear.do 
    swap 0 n
    maxHeap 0 (n - 1)

maxHeap :: Int -> Int -> SArray Int ()
maxHeap root bottom = Linear.do
  Ur child <- maxIndex left right bottom
  Ur root' <- maxIndex root child bottom
  if root' == root then
    return ()
  else Linear.do
    swap root' root
    maxHeap child bottom
  where
    left  = (root * 2) + 1;
    right = (root * 2) + 2;

maxIndex :: Int -> Int -> Int -> SArray Int (Ur Int)
maxIndex i j bottom
  | j > bottom = return (Ur i)
  | otherwise = Linear.do 
      Ur ival <- state (Array.get i)
      Ur jval <- state (Array.get j)
      return $ Ur (if ival >= jval then i else j)

