{-

    Problem 8
    
    (**) Eliminate consecutive duplicates of list elements.

    If a list contains repeated elements they should be replaced with 
    a single copy of the element. The order of the elements should not 
    be changed.

    Example:

    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)

    Example in Haskell:

    > compress "aaaabccaadeeee"
    "abcade"    

-}
import Data.List (group)

compress :: Eq a => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:y:zs) | x == y    =     compress (y:zs)
                  | otherwise = x : compress (y:zs)
                  
-- other methods from H99 site -------------------------------------------

compress_a :: Eq a => [a] -> [a]
-- 'group' groups like elements in a list, 'head' then takes the
-- first element in each grouping
compress_a = map head . group

compress_b xs = map head $ group xs

-- note that GHC removes the Maybe's
compress_c xs = foldr f (const []) xs Nothing
  where
    f x r a@(Just q) | x == q = r a
    f x r _          = x : r (Just x)
    
compress_d :: Eq a => [a] -> [a]
compress_d x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x
    
-- simple example    
compress_e []     = []
compress_e (x:xs) = x : (compress $ dropWhile (== x) xs)    

-- using fold left
compress_f x = foldl (\a b -> if (last a) == b then a else a ++ [b]) 
                [head x] x

compress_g x = reverse $ foldl (\a b -> if (head a) == b then a else b:a) 
                [head x] x
              
-- tests -----------------------------------------------------------------

testCharLst f = f "aaaabccaadeeee" == "abcade"

tests = testCharLst compress   && testCharLst compress_a
     && testCharLst compress_b && testCharLst compress_c
     && testCharLst compress_d && testCharLst compress_e
     && testCharLst compress_f && testCharLst compress_g
