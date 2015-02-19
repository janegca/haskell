-- Sort operations
module SortOps where

import Data.List (partition)

{- References:
    [DM]: 'Discrete Math Using a Computer', 
    [RC]: http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Haskell
-}

-- return True if the given list is sorted [DM]
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted (x:[])   = True 
isSorted (x:y:zs) = x < y && isSorted (y:zs)

-- merge two sorted lists [DM]
merge :: Ord a => [a] -> [a] -> [a]
merge xs []  = xs
merge [] ys  = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


-- sorts in order based on predicate [RC]
qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort p (x:xs) = qsort p ys ++ x : qsort p zs 
                 where (ys, zs) = partition (p x) xs
qsort _ [] = []

-- split a list into gt, lt halves and recurse on each half [DM]
quicksort :: Ord a => [a] -> [a]
quicksort []            = []
quicksort (splitter:xs) =
    quicksort [y | y <- xs, y<splitter]
    ++ [splitter]
    ++ quicksort [y | y <- xs, y>=splitter]

