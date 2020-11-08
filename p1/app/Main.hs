module Main where

import Criterion.Main
import Test.QuickCheck

import MergeSort ( mergeSort )
import InsertionSort ( insertionSort )

arbitraryIntVectorOf :: Int -> IO [Int]
arbitraryIntVectorOf n = generate (vectorOf n arbitrary)

benchSorts :: Int -> Benchmark
benchSorts n = bgroup (show n)
    [ env (arbitraryIntVectorOf n) (bench "merge" . whnf mergeSort)
    , env (arbitraryIntVectorOf n) (bench "insertion" . whnf insertionSort)
    ]

main = do
    defaultMain $ map benchSorts [0, 1, 2, 3, 4, 5, 10, 20, 35, 50, 75, 100, 200, 500, 1000, 2000, 5000, 10000]
