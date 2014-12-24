-- 03.06 Review - Recursion
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

{-  
   15. Write a function that takes two lists, and returns a list of values
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
    
{- ------------------------------------------------------------------------
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

{- -----------------------------------------------------------------------
    17. Write a recursive function that determines whether a list is
        sorted.
-}
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted (x:[])   = True 
isSorted (x:y:zs) = x < y && isSorted (y:zs)

{- -----------------------------------------------------------------------
    18. Show that the definition of factorial using foldr always produces
        the same result as the recursive definition given in the previous
        section.
        
        Base Case:
            foldr (*) 1 [] = 1      -- def of foldr, returns zero value
                                    -- for empty list argument
            factorial 0 = 1         -- def of factorial
            
        Recursive Case:
            Note that the definition of factorial
                factorial n+1 = n+1 * factorial n
            ==> factorial n   = n   * factorial n-1
            
            Let k = n - 1,
        
              foldr (*) 1 [1..n]    -- arg is list excluding the base case
            = foldr (*) 1 (1 :(2 :(3 : ... :( n : []))))  -- def of list
            = (1 * (2 * (3 * ... *( n * 1 ))))            -- apply foldr
            = (1 * (2 * (3 * ... * n)))       -- arithmetic
            = n * (1 * (2 * (3 * ... * k)))   -- assoc. property of mult    
            = n * factorial n-1               -- def of factorial           
-}
{- -----------------------------------------------------------------------
   19. Using recursion, define last, a function that takes a list and
       returns a Maybe type that is Nothing if the list is empty
    
-}
last' :: [a] -> Maybe a
last' []     = Nothing
last' (x:[]) = Just x
last' (x:xs) = last' xs

{- -----------------------------------------------------------------------
    20. Using recursion, write two functions that expect a string containing
        a number that contains a decimal point (for example, 23.455).
        
        The first function returns the whole part of the number (i.e., the
        part to the left of the decimal point). The second function 
        returns the fractional part (the part to the right of the decimal 
        point).
-}
wholeNum :: String -> String
wholeNum []     = ""
wholeNum (x:xs) | x == '.' = []
                | otherwise = x : wholeNum xs
               
fracNum :: String -> String
fracNum [] = ""
fracNum (x:xs) | x == '.'  = xs
               | otherwise = fracNum xs
               
