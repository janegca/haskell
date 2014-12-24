-- 03.06 Review - Recursion
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

{- 15. Write a function that takes two lists, and returns a list of values
       that appear in both lists. The function should have type 
       
            intersection :: Eq a => [a] -> [a] -> [a].
            
       Example:
            Main> intersection [0..10] [4..10]
           [4,5,6,7,8,9,10]

-}
intersection :: Eq a => [a] -> [a] -> [a]
intersection xs [] = []
intersection [] ys = []
intersection (x:xs) (y:ys)
    | (elem x ys) && (elem y xs) = x : y : intersection xs ys
    | elem x ys                  = x : intersection xs ys
    | elem y xs                  = y : intersection xs ys
    | otherwise                  = intersection xs ys
    
{-
    16. Write a function that takes two lists, and returns True if all the
        elements of the first list also occur in the other. The function 
        should have type:
        
            isSubset :: Eq a => [a] -> [a] -> Bool
            
        Example:
        
            Main> isSubset [3,4] [1..5]
            True
            Main> isSubset [7,8] [1..5]
            False
        
-}    
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs []     = False
isSubset [] ys     = True
isSubset (x:xs) ys = elem x ys && isSubset xs ys

{-
    17. Write a recursive function that determines whether a list is
        sorted.
-}
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted (x:[])   = True 
isSorted (x:y:zs) = x < y && isSorted (y:zs)

