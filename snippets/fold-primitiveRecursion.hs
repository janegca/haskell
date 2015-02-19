-- Hutton Fold Tutorial - Primitive Recursion
-- References:
--      [CIS194]: CIS 194: Introduction to Haskell (Fall 2014)
{-
    Recall that the following two equations capture the following
    simple pattern of recursion for defining a function that
    processes the elements of a list
    
        h []     = v
        h (x:xs) = g x (h xs)
        
    All such functions can be redefined as: h = fold g v
    
    We can generalize on this by first introducing an argument 'y'
    for the function h
        h y []     = f y
        h y (x:xs) = g y x (h y xs)
        
    And by application of the universal property, this can
    be redefined as
        h y = fold (g y) (f y)
    
    For a second step, we introduce an argument, xs, for the
    function g, giving the pattern
        h y []      = f y
        h y (x:xs)  = g y x xs (h y xs)
        
    This pattern is called 'primitive recursion'
    
    To redefine a primitive pattern to a fold we must solve
        h y = fold i j  where i is a function and j is a value
        
    This is not possible directly but is possible indireclty with
        k y xs = (h y xs, xs)
        
    which, we find by applying the universal property, is equal to
        k y [] = j
        k y (x:xs) = i x (k y xs)
        
    From the first equation, we get j = (f y, [])
    From the second equation, we calculate the definition of i as
        k y (x:xs )        = i x (k y xs)
    ==> (h y (x:xs), x:xs) = i x (h y xs, xs)               def of k
    ==> (g y x xs (h y xs), x:xs) = i x (h y xs, xs)        def of h
    ==> (g y x xs z, x:xs) = i x (z, xs)      gen. h y xs to z
    
    So
        k y = fold i j
            where
                i x (z, xs) = (g y x xs z, x:xs)
                j           = (f y, [])
                
    And the recursive function itself becomes
        h y = fst . k y
        
    This is the intuition behind Church numbers in lambda calculus
    as 3 = succ (succ (succ zero)) is \f x -> f(f(f x)) in that
    the two constructors, succ and zero, are replaced,
    succ is replaced by the fold op 'f' and zero is replaced by 'x'
    
-}

{-
    COMPOSING FUNCTIONS WITH FOLD
        the function composition operator (.) replaces the list
        concat operator (:) and the id function replaces the
        empty list []
-}
fold :: (a -> b -> b) -> b -> ([a] -> b)
fold f v [] = v
fold f v (x:xs ) = f x (fold f v xs)

compose :: [a -> a] -> (a -> a)
compose = fold (.) id

{-
    the natural definition of the sum function using a fold is
        sum = fold (+) 0
    this processes the numbers in the list from right to left
    
    We can right another function, suml, that will process the
    numbers from left to right BUT it requires an explicit recursive
    helper function that uses an accumulator (n)
-}
sum' :: [Int] -> Int
sum' = fold (+) 0

suml :: [Int] -> Int
suml xs = suml' xs 0
    where 
        suml' [] n     = n
        suml' (x:xs) n = suml' xs (n + x)
        
{-
    sum and suml give the same result when applied to the same list
    but suml has the potential to be more efficient because it can
    be modified to run in constant space; we can re-define the 
    helper function suml' to use fold
    
        suml' :: [Int] -> (Int -> Int)
        suml' []     = v
        suml' (x:Xs) = f x (sum xs)
        
    From the first equation, we get v = id
    From the second equation, we calculate f as:
        suml' (x:xs) = f x (suml' f xs)
    ==> suml' (x:xs) n = f x (suml' f xs) n             functions
    ==> suml' xs (n+x) = f x (suml' f xs) n             def suml'
    ==> g (n+x) = f x g n                          gen suml' to g
    ==> f = \x g -> (\n g -> (n + x))                   functions
    
    which summarizes to
        suml' = fold (\x g -> (\n -> g (n+x)) id
        
    This definition states that suml' processes a list by replacing the 
    empty list [] with the identity function id on lists, and each constructor
    (:) by the function that takes a number x and a function g, and returns
    the function that takes an accumulator value n and returns the result of
    applying g to the new accumulator value n + x    
    
    ie f takes two args: a list element 'x' and a function 'g n'
       g n takes one arg, 'n', the accumulator, which is added
       to every x
       
       right associative:  f x g n -> f(x (g n))
       
    The structuring of suml' as [Int] -> (Int -> Int) is crucial to
    defining suml' in terms of a fold
    
    THE ORDERING IN WHICH ELEMENTS ARE PROCESSED DEPENDS ON THE 
    ARGUMENTS THEMSELVES AND NOT FOLD
-}      

{-
    FOLDL
        generalizing suml' to get foldl

            foldl :: (b -> a -> b) -> b -> ([a] -> b)
            foldl f v [] = v
            foldl f v (x:xs) = foldl f (f v x) xs
            
        using this, we can redfine suml = foldl (+) 0
        
        Other primitive functions can also be redefined
-}    

-- this reverse is more efficient than our previous definition
-- using fold as we've avoided the expensive ++ operation
reverse' :: [a] -> [a]
reverse' = foldl (\ys x -> x : ys) []

-- other examples of functions as folds from [CIS194]
product' :: [Int] -> Int
product' []     = 1                 -- zero value => 1  (identity for mult)
product' (x:xs) = x * product' xs   -- operation  => (*)

product'' :: [Int] -> Int
product'' = foldr (*) 1             -- implicit recursion

length' :: [a] -> Int
length' []     = 0                  -- zero value => 0
length' (_:xs) = 1 + length' xs     -- operation  => (+1)

length'' :: [a] -> Int
length'' = foldr (\_ -> (+1)) 0     -- implicit recursion



