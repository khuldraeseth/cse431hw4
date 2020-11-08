module InsertionSort
    ( insertionSort
    ) where

import Data.List (partition)

insert :: (Ord a) => a -> [a] -> [a]
insert x xs = lt ++ [x] ++ ge
    where (lt, ge) = partition (<x) xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []
