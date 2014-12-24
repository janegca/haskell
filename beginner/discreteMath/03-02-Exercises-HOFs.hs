-- 03-02-Exercises-HOFs.hs
-- Exercises with higher order functions

{-
    11. Write foldrWith, a function that behaves like foldr except
        that it takes a function of three arguments and two lists.
        
        Note: like zipWith, applies a function to two lists but
              also reduces them to a single value
              
              Can't come up with a working example
-}
-- based on provided solution
foldrWith :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldrWith f z [] _  = z
foldrWith f z _ []  = z
foldrWith f z (x:xs) (y:ys) = f x y (foldrWith f z xs ys)

{-
    13. Using foldr, write a function mappend such that
            mappend f xs = concat (map f xs)
    
    Note: this is the same as the Prelude function 'concatMap'
          which maps a function over a list and concatenates
          the results
          
          Examples from concatMap examples on zvon.org:
          
          Main> mappend  (enumFromTo 1) [1,3,5] 
          [1,1,2,3,1,2,3,4,5]
          
          Main> mappend  (\x -> [(x,x+2,x/2)]) [1,3,5] 
         [(1.0,3.0,0.5),(3.0,5.0,1.5),(5.0,7.0,2.5)] 
         
         It appears the 'function' must be a 'list generating function'
         i.e. a function that creates a new list for every element in
              the given list
         The result is then the concatenation of the newly generated
         lists.
-}
-- provided solution
mappend :: (a -> [b]) -> [a] -> [b]
mappend f xs = foldr fun [] xs
    where fun x acc = f x ++ acc

{-  
    13. Write removeDuplicates, a function that takes a list and removes
        all of its duplicate elements
-}   
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) | elem x xs  = removeDuplicates xs
                        | otherwise = x : removeDuplicates xs
                        
{-
    14. Write a recursive function that takes a value and a list of values
        and returns True if the value is in the list and False otherwise
-}              
member :: Eq a => a -> [a] -> Bool
member a (x:xs) | a == x = True
                | otherwise = member a xs
member a [] = false

                




    

