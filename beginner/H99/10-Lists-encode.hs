{-
    Problem 10
    
    (*) Run-length encoding of a list. Use the result of problem P09 to 
        implement the so-called run-length encoding data compression 
        method. Consecutive duplicates of elements are encoded as lists
        (N E) where N is the number of duplicates of the element E.

    Example:

    * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

    Example in Haskell:

    encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]    

-}

import Data.List (group)        -- library function for 'pack'
import Control.Arrow

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

-- other methods from H99 site -------------------------------------------

encode_a xs = [(length x, head x) | x <- group xs]

-- using the (&&&) arrow operator for tuples (requires import)
encode_b :: Eq a => [a] -> [(Int, a)]
encode_b xs = map (length &&& head) $ group xs

-- using foldr
encode_c xs = (enc . group) xs
	where 
        enc = foldr (\x acc -> (length x, head x) : acc) []
        
-- without higher order functions
encode_d []     = []
encode_d (x:xs) = encode' 1 x xs 
    where
        encode' n x [] = [(n, x)]
        encode' n x (y:ys)
            | x == y    = encode' (n + 1) x ys
            | otherwise = (n, x) : encode' 1 y ys        

-- tests -----------------------------------------------------------------

testCharLst f =  f "aaaabccaadeeee" 
              == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]   

tests = testCharLst encode   && testCharLst encode_a
     && testCharLst encode_b              