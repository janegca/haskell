{-

    Problem 3
    
    (*) Find the K'th element of a list. The first element in the 
    list is [at index] number 1.

    Example:

    * (element-at '(a b c d e) 3)
    c

    Example in Haskell:

    Prelude> elementAt [1,2,3] 2
    2
    Prelude> elementAt "haskell" 5
    'e'    

-}

elementAt :: [a] -> Int -> a
elementAt lst index = if index <= 0
                      then error "Index out of bounds"
                      else f lst 1
    where
        f (x:xs) n | n == index = x
                   | otherwise  = f xs (n+1)
        f [] _ = error "Index out of bounds"

-- alternative methods
elementAt_a xs n = xs !! (n - 1)
elementAt_b xs n = last (take n xs)     -- see elementAt_f for point free

elementAt_c xs n = if n > 0
                   then  head [ x | (x,i) <- zip xs [1..], i == n]
                   else error "Index out of bounds"
                   
elementAt_d xs n 
    = if n > 0
      then fst . foldr1 (const id)      -- get first entry in pair
               . filter ((== n) . snd)  -- filter on req'd index
               $ zip xs [1..]           -- pair with pos index
      else error "Index out of bounds"

-- alternative methods given on H99 site
elementAt_e :: [a] -> Int -> a
elementAt_e [] _     = error "Index out of bounds"
elementAt_e (x:_)  1 = x
elementAt_e (_:xs) n 
    | n > 0    = elementAt_e xs (n - 1)
    |otherwise = error "Index out of bounds"

elementAt_f xs n 
    | n <= 0        = error "Index out of bounds"
    | length xs < n = error "Index out of bounds"
    | otherwise     = fst . last $ zip xs [1..n]    
       
elementAt_g = flip $ (last .) . take  

-- tests -----------------------------------------------------------------
testIntList  fn = fn [1,2,3,4] 2 == 2
testCharList fn = fn "haskell" 5 == 'e'

tests = 
       testIntList elementAt   && testCharList elementAt
    && testIntList elementAt_a && testCharList elementAt_a
    && testIntList elementAt_b && testCharList elementAt_b
    && testIntList elementAt_c && testCharList elementAt_c
    && testIntList elementAt_d && testCharList elementAt_d
    && testIntList elementAt_e && testCharList elementAt_e
    && testIntList elementAt_f && testCharList elementAt_f
    && testIntList elementAt_g && testCharList elementAt_g

