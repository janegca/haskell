{-
    Problem 13
    
    (**) Run-length encoding of a list (direct solution).

    Implement the so-called run-length encoding data compression method 
    directly. I.e. don't explicitly create the sublists containing the 
    duplicates, as in problem 9, but only count them. As in problem P11, 
    simplify the result list by replacing the singleton lists (1 X) by X.

    Example:

    * (encode-direct '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))

    Example in Haskell:

    P13> encodeDirect "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']    

-}
-- run length encoded list data type from Problem 11
data Encoded a = Single a
               | Multiple Int a
    deriving (Eq, Show)

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect []       = []
encodeDirect (x:xs)   = 
    let (v, rest) = span (== x) xs
        count     = 1 + (length v)
    in makeElem count x : encodeDirect rest
    where 
        makeElem 1 x = Single x
        makeElem n x = Multiple n x
 
-- revised after seeing H99 solutions 
encodeDirect_c :: Eq a => [a] -> [Encoded a]
encodeDirect_c []       = []
encodeDirect_c (x:xs)   = makeElem count x : encodeDirect_c rest
    where 
        -- don't need the 'let'
        (v, rest) = span (== x) xs
        count     = 1 + (length v)
        
        makeElem 1 x = Single x
        makeElem n x = Multiple n x
        
        
                            
-- other methods from H99 site ------------------------------------------
encodeDirect_a :: (Eq a) => [a] -> [Encoded a]
encodeDirect_a [] = []
encodeDirect_a (x:xs) = encodeDirect' 1 x xs
    where
        encodeDirect' n y [] = [encodeElement n y]
        encodeDirect' n y (x:xs) 
            | y == x    = encodeDirect' (n+1) y xs
            | otherwise = encodeElement n y : (encodeDirect' 1 x xs)
            
        encodeElement 1 y = Single y
        encodeElement n y = Multiple n y
      
encodeDirect_b :: (Eq a)=> [a] -> [Encoded a]
encodeDirect_b [] = []
encodeDirect_b (x:xs)
    | count == 1  = (Single x) : (encodeDirect_b xs)
    | otherwise   = (Multiple count x) : (encodeDirect_b rest)
    where
        (matched, rest) = span (==x) xs
        count           = 1 + (length matched)      

-- tests ----------------------------------------------------------------

testLst f =    f "aaaabccaadeeee"
            == [Multiple 4 'a',Single 'b',Multiple 2 'c',
                Multiple 2 'a',Single 'd',Multiple 4 'e']                           
                
tests = testLst encodeDirect   && testLst encodeDirect_a        
     && testLst encodeDirect_b && testLst encodeDirect_c   