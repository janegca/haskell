{-
    Problem 18
    
    (**) Extract a slice from a list.

    Given two indices, i and k, the slice is the list containing the 
    elements between the i'th and k'th element of the original list 
    (both limits included). Start counting the elements with 1.

    Example:

    * (slice '(a b c d e f g h i k) 3 7)
    (C D E F G)

    Example in Haskell:

    *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    "cdefg"    

-}
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k-i + 1) $ drop (i-1) xs

-- other methods from H99 site ------------------------------------------
slice_a xs i k = [x | (x,j) <- zip xs [1..k], i <= j]

slice_b xs i k = fst $ unzip $ filter ((>=i) . snd) $ zip xs [1..k]

slice_c xs i k = drop (i-1) $ take k xs

slice_d xs i j = map snd
               $ filter (\(x,_) -> x >= i && x <= j)
               $ zip [1..] xs

-- tests ----------------------------------------------------------------
tstLst f = (f ['a','b','c','d','e','f','g','h','i','k'] 3 7) == "cdefg"

tests = tstLst slice   && tstLst slice_a && tstLst slice_b
     && tstLst slice_c && tstLst slice_d


