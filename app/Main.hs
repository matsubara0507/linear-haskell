{-# LANGUAGE BangPatterns, NoImplicitPrelude, LinearTypes #-}

module Main (main) where

import Prelude hiding ((>>))
import Control.Functor.Linear ((>>))


import Control.Monad hiding ((>>))
import System.Random
import Test.Tasty.Bench

import Data.List as List
import Sort
import qualified Strict.Sort as S
import qualified Linear.Sort as L
import qualified Linear.StateSort as LS

mkBench :: String -> ([Int] -> [Int]) -> [Int] ->  Benchmark
mkBench s f xs = bench s $ nf f xs
{-# INLINE mkBench #-}

main :: IO ()
main = do
  !r1 <- randomList 10000 10000
  -- !r2 <- randomList 100000 100000
  -- !r3 <- randomList 1000000 1000000
  defaultMain
    [ mkBench "heap sort (10^4)" heapSort r1
    -- , mkBench "heap sort (10^5)" heapSort r2
    -- , mkBench "heap sort (10^6)" heapSort r3
    -- , mkBench "strict heap sort (10^4)" S.heapSort r1
    -- , mkBench "strict heap sort (10^5)" S.heapSort r2
    -- , mkBench "strict heap sort (10^6)" S.heapSort r3
    -- , mkBench "linear heap sort (10^4)" L.heapSort r1
    -- , mkBench "linear heap sort (10^5)" L.heapSort r2
    , mkBench "linear heap sort (10^4)" LS.heapSort r1
    -- , mkBench "linear heap sort (10^5)" LS.heapSort r2
    -- , mkBench "linear heap sort (10^6)" LS.heapSort r3
    -- , mkBench "linear quick sort (10^4)" L.quickSort r1
    -- , mkBench "linear quick sort (10^4)" LS.quickSort r1
    -- , mkBench "bubble sort (10^4)" bubbleSort r1
    -- , mkBench "linear bubble sort (10^4)" LS.bubbleSort r1
    -- , mkBench "linear swap (10^4)" (LS.execSArray (LS.swap 0 1 >> LS.swap 0 1)) r1 
    , mkBench "sort (10^4)" List.sort r1
    ]

randomList :: Int -> Int -> IO [Int]
randomList n boundary = replicateM n (getStdRandom (randomR (0, boundary)))
