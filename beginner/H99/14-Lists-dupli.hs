{-

    Problem 14
    
    (*) Duplicate the elements of a list.

    Example:

    * (dupli '(a b c c d))
    (A A B B C C C C D D)

    Example in Haskell:

    > dupli [1, 2, 3]
    [1,1,2,2,3,3]    
-}
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

-- other methods from H99 site -----------------------------------------
dupli_a list = concat [[x,x] | x <- list]     -- list comprehension

dupli_b xs = xs >>= (\x -> [x,x])             -- list monad

dupli_c = concatMap (\x -> [x,x])             -- using concatMap
dupli_d = concatMap (replicate 2)

dupli_e = foldl (\acc x -> acc ++ [x,x]) []   -- using foldl

dupli_f = foldr (\x -> (x:) . (x:)) []        -- using foldr

-- tests ---------------------------------------------------------------
tstList f = f [1,2,3] == [1,1,2,2,3,3]

tests = tstList dupli   && tstList dupli_a && tstList dupli_b
     && tstList dupli_c && tstList dupli_d && tstList dupli_e
     && tstList dupli_f