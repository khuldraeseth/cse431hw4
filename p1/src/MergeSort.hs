module MergeSort
    ( mergeSort
    ) where

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | y < x     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

unterleave :: [a] -> ([a], [a])
unterleave = foldr go ([], [])
    where go x (xs, ys) = (ys, x:xs)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge as' bs'
    where (as, bs) = unterleave xs
          as' = mergeSort as
          bs' = mergeSort bs
