{-
    Problem 17
    
    (*) Split a list into two parts; the length of the first part is given.

    Do not use any predefined predicates.

    Example:

    * (split '(a b c d e f g h i k) 3)
    ( (A B C) (D E F G H I K))

    Example in Haskell:

    *Main> split "abcdefghik" 3
    ("abc", "defghik")    

-}
split :: [a] -> Int -> ([a],[a])
split lst n = (map fst before, map fst after)
    where
        xs     = zip lst [0..]
        before = filter ((< n) . snd) xs
        after  = filter ((>=n) . snd) xs
        
split_a lst n = ([ x | (x,i) <- zlst, i < n],
                 [ x | (x,i) <- zlst, i >= n] )   
    where
        zlst = zip lst [0..]
        
-- other methods from H99 site ------------------------------------------
split_b :: [a] -> Int -> ([a], [a])
split_b []         _             = ([], [])
split_b l@(x : xs) n | n > 0     = (x : ys, zs)
                     | otherwise = ([], l)
    where (ys,zs) = split_b xs (n - 1)        
    
-- using built-in predicates --------------------------------------------
split_c xs n = (take n xs, drop n xs)    

split_d = flip splitAt


        
-- tests ----------------------------------------------------------------
tstLst f = f "abcdefghik" 3 == ("abc", "defghik")

tests = tstLst split   && tstLst split_a && tstLst split_b
     && tstLst split_c && tstLst split_d