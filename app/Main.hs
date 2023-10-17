{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Prelude

import Control.Monad
import System.Random
import Test.Tasty.Bench

import Sort
import qualified Strict.Sort as S
import qualified Linear.Sort as L
import qualified Linear.StateSort as LS

mkBench :: String -> ([Int] -> [Int]) -> [Int] ->  Benchmark
mkBench s f xs = bench s $ nf f xs

main :: IO ()
main = do
  !r1 <- randomList 10000 10000
  !r2 <- randomList 100000 100000
  !r3 <- randomList 1000000 1000000
  defaultMain
    [ mkBench "heap sort (10^4)" heapSort r1
    , mkBench "heap sort (10^5)" heapSort r2
    -- , mkBench "heap sort (10^6)" heapSort r3
    -- , mkBench "strict heap sort (10^4)" S.heapSort r1
    -- , mkBench "strict heap sort (10^5)" S.heapSort r2
    -- , mkBench "strict heap sort (10^6)" S.heapSort r3
    -- , mkBench "linear heap sort (10^4)" L.heapSort r1
    -- , mkBench "linear heap sort (10^5)" L.heapSort r2
    , mkBench "linear heap sort (10^4)" LS.heapSort r1
    , mkBench "linear heap sort (10^5)" LS.heapSort r2
    -- , mkBench "linear heap sort (10^6)" LS.heapSort r3
    , mkBench "linear quick sort (10^5)" LS.quickSort r2
    ]

randomList :: Int -> Int -> IO [Int]
randomList n boundary = replicateM n (getStdRandom (randomR (0, boundary)))
