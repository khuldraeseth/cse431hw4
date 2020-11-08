module Main where

import Criterion.Main
import Test.QuickCheck

import TimSort (timSort)

arbitraryIntVectorOf :: Int -> IO [Int]
arbitraryIntVectorOf n = generate (vectorOf n arbitrary)

benchTim :: Int -> Int -> Benchmark
benchTim n m = env (arbitraryIntVectorOf n) (bench ("tim" ++ show m) . whnf (timSort m))

benchSorts :: Int -> Benchmark
benchSorts n = bgroup (show n) $ map (benchTim n) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 35, 50, 75, 100, 200, 500, 1000]

main = do
    defaultMain $ map benchSorts [0, 1, 2, 3, 4, 5, 10, 20, 35, 50, 75, 100, 200, 500, 1000, 2000, 5000, 10000]
