{-
    Problem 16
    
    (**) Drop every N'th element from a list.

    Example:

    * (drop '(a b c d e f g h i k) 3)
    (A B D E G H K)

    Example in Haskell:

    *Main> dropEvery "abcdefghik" 3
    "abdeghk"    

-}
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter ((/= 0) . (`mod` n) . snd)
                         $ zip xs [1..]
                         
                        
-- other methods from H99 site -------------------------------------------
dropEvery_a :: [a] -> Int -> [a]
dropEvery_a list count = helper list count count
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))
        
-- similar but using a closure
dropEvery_b :: [a] -> Int -> [a]
dropEvery_b xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)

-- another map/filter/zip approach, uses lambda for filter expression
dropEvery_c xs n = map fst $ filter (\(x,i) -> i `mod` n /= 0) 
                           $ zip xs [1..]
                           
-- simplified version of above map/filter/zip    
-- Note: 'cycle' sets second (position) values to from 1 to n
--       continually reapplying
--       eg zip [1..10] (cycle [1..3])
--       ==> [(1,1),(2,2),(3,3),
--            (4,1),(5,2),(6,3),
--            (7,1),(8,2),(9,3),
--            (10,1)]
--      allows us to drop the 'mod' calculation

dropEvery_d xs n = map fst $ filter ((n/=) . snd) 
                           $ zip xs (cycle [1..n])                                 


-- tests -----------------------------------------------------------------
tstLst f = f "abcdefghik" 3 == "abdeghk"    

tests = tstLst dropEvery   && tstLst dropEvery_a && tstLst dropEvery_b
     && tstLst dropEvery_c && tstLst dropEvery_d