{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}

module Linear.StateSort where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import Prelude.Linear hiding (partition, foldr)
import Control.Functor.Linear as Linear
import Data.Foldable (Foldable, foldr)

type SArray a = State (Array a)

execSArray :: SArray Int () -> [Int] -> [Int]
execSArray f xs = unur $ Array.fromList xs (Array.toList . execState f)

swap :: Int -> Int -> SArray a ()
swap i j = Linear.do
  Ur ival <- state (Array.get i)
  Ur jval <- state (Array.get j)
  modify (Array.set i jval . Array.set j ival)

-- traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
-- traverse_ f = foldr c (pure ())
--   where c x k = f x *> k
--         {-# INLINE c #-}

-- for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
-- {-# INLINE for_ #-}
-- for_ = flip traverse_

upto :: Int -> Int -> (Int -> SArray a ()) -> SArray a ()
upto start end f = 
  foldM (\() n -> move n & \(Ur m) -> f m) () [start..end]
  -- foldM (\() n -> f n) () [start..end]

downto :: Int -> Int -> (Int -> SArray a ()) -> SArray a ()
downto start end f = 
  foldM (\() n -> move n & \(Ur m) -> f m) () [start, (start-1)..end]

heapSort :: [Int] -> [Int]
heapSort = execSArray $ Linear.do
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
    left  = (root * 2) + 1
    right = (root * 2) + 2

heapSort' :: [Int] -> [Int]
heapSort' = execSArray $ Linear.do
  Ur len <- state Array.size
  downto ((len - 1) `div` 2) 0 $ \n ->
    maxHeap' n (len - 1)
  downto (len - 1) 1 $ \n -> Linear.do 
    swap 0 n
    maxHeap' 0 (n - 1)

maxHeap' :: Int -> Int -> SArray Int ()
maxHeap' root bottom = Linear.do
  Ur index <- maxIndex' root
  go root index
  where
    go :: Int -> Int -> SArray Int ()
    go root' index 
      | index == root' = return ()
      | otherwise = Linear.do
          swap root' index
          Ur index' <- maxIndex' index
          go index index'
    maxIndex' :: Int -> SArray Int (Ur Int)
    -- maxIndex' root' = Linear.do
    --   Ur child <- maxIndex left right bottom
    --   maxIndex root' child bottom
    maxIndex' root' =
      if left > bottom then
        return (Ur root')
      else Linear.do
        Ur rootv <- state (Array.get root')
        Ur leftv <- state (Array.get left)
        if right > bottom then
          return $ Ur (if rootv >= leftv then root' else left)
        else Linear.do
          Ur rightv <- state (Array.get right)
          if leftv >= rightv then
            return $ Ur (if rootv >= leftv then root' else left)
          else
            return $ Ur (if rootv >= rightv then root' else right)
      where
        left  = (root' * 2) + 1
        right = (root' * 2) + 2

maxIndex :: Int -> Int -> Int -> SArray Int (Ur Int)
maxIndex i j bottom
  | j > bottom = return (Ur i)
  | otherwise = Linear.do 
      Ur ival <- state (Array.get i)
      Ur jval <- state (Array.get j)
      return $ Ur (if ival >= jval then i else j)

bubbleSort :: [Int] -> [Int]
bubbleSort = execSArray $ Linear.do
  Ur len <- state Array.size
  upto 0 (len - 1) $ \i ->
    downto (len - 1) (i + 1) $ \j -> Linear.do
      Ur jval  <- state (Array.get j)
      Ur jval' <- state (Array.get (j - 1))
      if jval > jval' then
        return ()
      else
        swap j (j - 1)

quickSort :: [Int] -> [Int]
quickSort = execSArray $ Linear.do
  Ur len <- state Array.size
  go 0 (len - 1)
  where
    go :: Int -> Int -> SArray Int ()
    go lo hi 
      | lo >= hi = return ()
      | otherwise = Linear.do
        Ur pivot <- state (Array.get lo)
        Ur ix <- partition pivot lo hi
        swap lo ix
        go lo (ix - 1)
        go (ix + 1) hi

    partition :: Int -> Int -> Int -> SArray Int (Ur Int)
    partition pivot lx rx
      | rx < lx = return $ Ur (lx - 1)
      | otherwise = Linear.do
          Ur lval <- state (Array.get lx)
          Ur rval <- state (Array.get rx)
          case (lval <= pivot, pivot < rval) of
            (True,  True)  -> partition pivot (lx + 1) (rx - 1)
            (True,  False) -> partition pivot (lx + 1) rx
            (False, True)  -> partition pivot lx (rx - 1)
            (False, False) -> swap lx rx >> partition pivot (lx + 1) (rx - 1)
