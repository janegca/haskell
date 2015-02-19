-- Hutton Fold Tutorial - Using tuples

{-
    A SIMPLE EXAMPLE
        a function to calculate the sum and length of a list of numbers
        
-}

fold :: (a -> b -> b) -> b -> ([a] -> b)
fold f v [] = v
fold f v (x:xs ) = f x (fold f v xs)

sumlength :: [Int] -> (Int, Int)
sumlength xs = (sum xs, length xs)

-- we can convert this to a fold by directly using the sum and
-- length definitions (simpler as only one list traversal)
--
-- Note: 'n' is the value coming from the list, (x,y) is the accumulator
sumlength' :: [Int] -> (Int, Int)
sumlength' = fold (\n (x,y) -> (n + x, 1 + y)) (0,0)

{-
    A MORE INTERESTING EXAMPLE
    
    Let's take a look at dropwhile
        dropWhile :: (a -> Bool) -> [a] -> [a]
        dropWhile p []     = []
        dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs
        
    To transform it to a fold so 
        dropWhile p = fold f v
    we know, from the universal property that this is equivalent to
    the two equations:
        dropWhile p []     = []
        dropWhile p (x:xs) = f x (dropWhile p xs)
        
    We can easily see that v = []
    From the second equation we can attempt to calculate f
        dropWhile p (x:xs) = f x (dropWhile p xs)
    ==> if p x then dropWhile p xs else x:xs = f x (dropWhile p xs)
    ==> if p x then ys else x:xs = f x ys
    
    But we have a problem, 'xs' is free; so we can not 'directly'
    transform dropWhile to a fold BUT we can do so 'indirectly'
    using a more general definition that pairs up the results of
    applying dropWhile  p to a list
    
    Essentially dropWhile' returns the result of dropWhile p and the
    original list in a tuple
    eg dropWhile' odd [1,3,5,6,9,11] -> ([6,9,11], [1,3,5,6,9,11)
        
-}
dropWhile' :: (a -> Bool) -> ([a] -> ([a],[a]))
dropWhile' p xs = (dropWhile p xs, xs)
        
{-
    dropWhile' CAN be redefined as a fold
    
        dropWhile' p []     = v
        dropWhile' p (x:xs) = f x (dropWhile' p xs)
        
    From the first equation, we can see that v = ([],[])
    From the second equation:
        dropWhile' p (x:xs)         = f x (dropWhile' p xs)
    ==> (dropWhile' p (x:xs), x:xs) = f x (dropWhile p xs, xs)
    ==> (if p x then dropWhile p xs else x:xs, xs) = 
                f x (dropWhile p xs, xs)
    ==> (if p x then ys else x:xs, x:xs) = f x (ys, xs)
    
    the last line is a valid definition of 'f' as all the variables
    are bound
    
    So, we get the following definition
-}        
dropWhile'' p = fold f v
    where f x (ys, xs) = (if p x then ys else x:xs, x:xs)
          v = ([],[])
          
-- and we can now define dropWhile in terms of a fold
dropWhileFold p = fst . dropWhile'' p

         