{-
    Problem 15
    
    (**) Replicate the elements of a list a given number of times.

    Example:

    * (repli '(a b c) 3)
    (A A A B B B C C C)

    Example in Haskell:

    > repli "abc" 3
    "aaabbbccc"    

-}
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- other methods from H99 site ------------------------------------------
repli_a = flip $ concatMap . replicate          -- point free style

repli_b xs n = concatMap (take n . repeat) xs   -- w/o replicate

repli_c xs n = xs >>= replicate n               -- list monad

repli_d :: [a] -> Int -> [a]                      -- using foldl
repli_d xs n = foldl (\acc e -> acc ++ repli' e n) [] xs
    where
      repli' _ 0 = []
      repli' x n = x : repli' x (n-1)
      
repli_e :: [a] -> Int -> [a]                      -- using foldr
repli_e [] _ = []
repli_e (x:xs) n = foldr (const (x:)) (repli_e xs n) [1..n]      

-- tests ----------------------------------------------------------------
tstLst f = (f "abc" 3) == "aaabbbccc"

tests = tstLst repli    && tstLst repli_a && tstLst repli_b
     && tstLst repli_c && tstLst repli_d && tstLst repli_e
     