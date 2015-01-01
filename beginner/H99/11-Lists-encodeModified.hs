{-
    Problem 11
    
    (*) Modified run-length encoding.

    Modify the result of problem 10 in such a way that if an element has 
    no duplicates it is simply copied into the result list. Only elements 
    with duplicates are transferred as (N E) lists.

    Example:

    * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))

    Example in Haskell:

    P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']    

-}
import Data.List (group)

-- based on the example of the required result, we need a new data type
data Encoded a = Single a
               | Multiple Int a
    deriving (Eq, Show)
    
-- encode from problem 10    
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group
    
-- NOTE: cleaner use of a helper function is shown in version _c below    
encodeModified :: Eq a => [a] -> [Encoded a] 
encodeModified xs = em (encode xs)
    where
        em (x:xs) | (fst x) == 1 = Single (snd x) : em xs
                  | otherwise = Multiple (fst x) (snd x) : em xs
        em _ = []
             
-- alternative methods ---------------------------------------------------
encodeModified_a :: Eq a => [a] -> [Encoded a]
encodeModified_a = map (\(a,b) -> if a == 1 then Single b else Multiple a b)
                 . encode             
       
encodeModified_b xs = [ if a == 1 then Single b else Multiple a b |
                        (a,b) <- encode xs]       
                        
-- other methods form H99 site -------------------------------------------
encodeModified_c :: Eq a => [a] -> [Encoded a]
encodeModified_c = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x         

encodeModified_d xs = [y | x <- group xs, 
                           let y = if (length x) == 1 
                                   then Single (head x) 
                                   else Multiple (length x) (head x)]      

-- tests -----------------------------------------------------------------

testLst f = f  "aaaabccaadeeee"
          == [Multiple 4 'a',Single 'b',Multiple 2 'c',
              Multiple 2 'a',Single 'd',Multiple 4 'e']            
              
tests = testLst encodeModified   && testLst encodeModified_a     
     && testLst encodeModified_b && testLst encodeModified_c
     && testLst encodeModified_d