module TimSort
    ( timSort
    ) where

import InsertionSort (insertionSort)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | y < x     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

unterleave :: [a] -> ([a], [a])
unterleave = foldr go ([], [])
    where go x (xs, ys) = (ys, x:xs)

timSort' :: (Ord a) => Int -> Int -> [a] -> [a]
timSort' n k xs
    | k <= n    = insertionSort xs
    | otherwise = merge as' bs'
    where (as, bs) = unterleave xs
          as' = timSort' n x as
          bs' = timSort' n y bs
          x = k `div` 2
          y = (k+1) `div` 2

timSort :: (Ord a) => Int -> [a] -> [a]
timSort n xs = timSort' n (length xs) xs
