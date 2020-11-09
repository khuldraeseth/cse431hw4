module Main where

import Criterion.Main
import Test.QuickCheck

import qualified Data.Set as S
import qualified Data.HashSet as HS

arbitraryIntVectorOf :: Int -> IO [Int]
arbitraryIntVectorOf n = generate (vectorOf n arbitrary)

benchSets :: Int -> Benchmark
benchSets n = bgroup (show n)
    [ env (arbitraryIntVectorOf n) (bench "Set" . whnf (foldr S.insert S.empty))
    , env (arbitraryIntVectorOf n) (bench "HashSet" . whnf (foldr HS.insert HS.empty))
    ]

main = do
    defaultMain $ map benchSets [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000]
