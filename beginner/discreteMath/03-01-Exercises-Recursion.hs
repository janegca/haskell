-- 03 Exercises involving Recursion
import Prelude hiding ( (!!), lookup )
import Data.List (isPrefixOf )

{- 1. Write a recursive function 
        copy :: [a] -> [a] 
      
      that copies its list argument. 
      For example, copy [2] = [2].
-}      
copy :: [a] -> [a]
copy []     = []
copy (x:xs) = x : copy xs

{- 2. Write a function inverse that takes a list of pairs and swaps the
      pair elements. For example,
        
        inverse [(1,2),(3,4)] ==> [(2,1),(4,3)]
-}
inverse :: [(a,b)] -> [(b,a)]
inverse []  = []
inverse ((a,b): ab) = (b,a) : inverse ab

{- 3. Write a function
        merge :: Ord a => [a] -> [a] -> [a]

      which takes two sorted lists and returns a sorted list containing 
      the elements of each.
-}
merge :: Ord a => [a] -> [a] -> [a]
merge xs []  = xs
merge [] ys  = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
    
{-
    4. Write (!!), a function that takes a natural number n and a list
    and selects the nth element of the list. List elements are indexed from
    0, not 1, and since the type of the incoming number does not prevent it
    from being out of range, the result should be a Maybe type. For example,
            [1,2,3]!!0 ==> Just 1
            [1,2,3]!!2 ==> Just 3
            [1,2,3]!!5 ==> Nothing
-}
(!!) :: Int -> [a] -> Maybe a
(!!) n []     = Nothing
(!!) 0 (x:xs) = Just x
(!!) n (x:xs) = (!!) (n-1) xs

{-
    5. Write a function lookup that takes a value and a list of pairs,
       and returns the second element of the pair that has the value as 
       its first element. Use a Maybe type to indicate whether the lookup
       succeeded. For example,
            lookup 5 [(1,2),(5,3)] ==> Just 3
            lookup 6 [(1,2),(5,3)] ==> Nothing
-}
lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup n ((x,y):xs) | n == x     = Just y
                    | otherwise  = lookup n xs
lookup _ _ = Nothing

{-
    6. Write a function that counts the number of times an element
       appears in a list.
-}
count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs) | n == x    = 1 + count n xs
               | otherwise = count n xs
          
{- 
    7. Write a function that takes a value e and a list of values xs and
       removes all occurrences of e from xs.
-}          
del :: Eq a => a -> [a] -> [a]
del e (x:xs) | e == x    = del e xs
             | otherwise = x : del e xs
del _ _ = []

{-
    8 Write a function
        f :: [a] -> [a]
      that removes alternating elements of its list argument, starting 
      with the first one. For examples, f [1,2,3,4,5,6,7] returns [2,4,6].
-}
removeAlt :: [a] -> [a]
removeAlt (x:y:[]) = [y]
removeAlt (x:y:zs) = y : removeAlt zs
removeAlt _ = []

{-
    9. Write a function 
            extract :: [Maybe a] -> [a] 
       
       that takes a list of Maybe values and returns the elements they
       contain. For example,
            
            extract [Just 3, Nothing, Just 7] = [3, 7].
-}
extract :: [Maybe a] -> [a]
extract []             = []
extract (Nothing : xs) = extract xs
extract (Just x : xs)  = x : extract xs
{-
    10. Write a function
            f :: String -> String -> Maybe Int
            
        that takes two strings. If the second string appears within the 
        first, it returns the index identifying where it starts. Indexes 
        start from 0. For example,
            f "abcde" "bc" ==> Just 1
            f "abcde" "fg" ==> Nothing
-}
f :: String -> String -> Maybe Int
f str [] = Nothing
f [] _  = Nothing
f str substr = if isPrefixOf substr str then Just 0 else find 1 str
    where
        find idx (x:xs) | isPrefixOf substr xs = Just idx
                        | otherwise            = find (idx + 1) xs
        find _ _ = Nothing
        
-- provided solution
f' :: String -> String -> Maybe Int
f' str1 str2 = loop str1 str2 0
    where
        loop :: String -> String -> Int -> Maybe Int
        loop [] s2 n = Nothing
        loop (x:xs) s2 n
            = if length s2 > length (x:xs)
              then Nothing
              else if take (length s2) (x:xs) == s2
                then Just n
                else loop xs s2 (n+1)    
             
             
             