{-
    Problem 9
    
    (**) Pack consecutive duplicates of list elements into sublists. 
         If a list contains repeated elements they should be placed in 
         separate sublists.

    Example:

    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))

    Example in Haskell:

    *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
                 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]    

    [NOTE: this is what the Data.List group method does]
-}

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = let a = g (head xs) xs
          in (fst a) : pack (snd a)
    where 
        g x xs = ( takeWhile (== x) xs, dropWhile (== x) xs )
        
-- other methods from H99 site -------------------------------------------
-- 'span' does what 'g' does above
pack_a (x:xs) = let (first,rest) = span (==x) xs
                in (x:first) : pack rest
pack_a [] = []        

pack_b :: (Eq a) => [a] -> [[a]]
pack_b [] = []
pack_b (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- using foldr
pack_c :: (Eq a) => [a] -> [[a]]
pack_c = foldr func []
    where func x []     = [[x]]
          func x (y:xs) =
              if x == (head y) then ((x:y):xs) else ([x]:y:xs)
              
-- a simple solution
pack_d :: (Eq a) => [a] -> [[a]]
pack_d [] = []
pack_d [x] = [[x]]
pack_d (x:xs) = if x `elem` (head (pack_d xs))
                then (x:(head (pack_d xs))):(tail (pack_d xs))
                else [x]:(pack_d xs)              

-- tests -----------------------------------------------------------------

lsta = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd',
        'e', 'e', 'e', 'e']
        
lstb = ["aaaa","b","cc","aa","d","eeee"]     
      
testCharLst f = f lsta  == lstb

tests = testCharLst pack   && testCharLst pack_a             
     && testCharLst pack_b && testCharLst pack_c
     && testCharLst pack_d          