{-
    Problem 19
    
    (**) Rotate a list N places to the left.

    Hint: Use the predefined functions length and (++).

    Examples:

    * (rotate '(a b c d e f g h) 3)
    (D E F G H A B C)

    * (rotate '(a b c d e f g h) -2)
    (G H A B C D E F)

    Examples in Haskell:

    *Main> rotate ['a','b','c','d','e','f','g','h'] 3
    "defghabc"
     
    *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
    "ghabcdef"    

-}
rotate :: [a] -> Int -> [a]
rotate lst n = let k = if n >= 0 then n else length lst + n
                   (xs, ys) = splitAt k lst
               in ys ++ xs
               
-- other methods from H99 site ---------------------------------------
rotate_a xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs
    
rotate_b xs n = take (length xs) $ drop (length xs + n) $ cycle xs

rotate_c :: [a] -> Int -> [a]
rotate_c [] _ = []
rotate_c x 0  = x
rotate_c x y
  | y > 0     = rotate_c (tail x ++ [head x]) (y-1)
  | otherwise = rotate_c (last x : init x) (y+1)
  
rotate_d xs n
    | n < 0     = rotate_d xs (n+len)
    | n > len   = rotate_d xs (n-len)
    | otherwise = let (f,s) = splitAt n xs in s ++ f
    where len = length xs  

-- tests -------------------------------------------------------------

tstLst   = ['a','b','c','d','e','f','g','h']
posTst f = f tstLst 3    == "defghabc"
negTst f = f tstLst (-2) == "ghabcdef"

tests = posTst rotate   && negTst rotate
     && posTst rotate_a && negTst rotate_a
     && posTst rotate_b && negTst rotate_b
     && posTst rotate_c && negTst rotate_c
     && posTst rotate_d && negTst rotate_d
     
     