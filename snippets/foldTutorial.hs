-- Hutton 1998 Fold Tutorial

{-
    Basic Definiton of a fold
    
    Given a function f of type a -> b -> b and a value 'v' of type 'b'
    the function 'fold f v' processes a list of type [a] to return
    a value of type 'b' by replacing the 'nil' constructor [] at the
    end of the list with the value 'v' and each 'cons' constrcutor (:)
    within the list by the function 'f'
    
    ie the two constructors for lists: [] and :, are replaced by other
       values and functions: [] -> v,  (:) -> f
-}
fold :: (a -> b -> b) -> b -> ([a] -> b)
fold f v [] = v
fold f v (x:xs ) = f x (fold f v xs)

-- examples of simple functions that can defined as folds
sum' :: [Int] -> Int
sum' = fold (+) 0

product' :: [Int] -> Int
product' = fold (*) 1

and' :: [Bool] -> Bool
and' = fold (&&) True

or' :: [Bool] -> Bool
or' = fold (||) False

{- defined in the Prelude
(++) :: [a] -> [a] -> [a]
(++) ys = fold (:) ys
-}

-- the replacement functions used above were existing ones
-- however, they can also be user defined
length' :: [a] -> Int
length' = fold (\x n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' = fold (\x xs -> xs ++ [x]) []

map' :: (a -> b) -> ([a] -> [b])
map' f = fold (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> ([a] -> [a])
filter' p = fold (\x xs -> if p x then x:xs else xs) []

{-
    UNIVERSAL PROPERTY OF FOLD  
    
        g []     = v
        g (x:xs) = f x (g xs)
        where g = fold f v
        
        So substituting 'g' into the two equations gives
        the definition of fold
        
        fold f v []     = v
        fold f v (x:xs) = f x (fold f v xs)
       
        Main use of the property is provide a 'proof'. 
        Any two functions that can be proven equal by induction can be
        proven equal by the Universal Property of the 'fold' operator
        providing both functions can be expressed using 'fold'
        
        Example: 
            (+1) . sum = fold (+) 1
            
            here the right hand side (f (+) 1) matches g = fold f v
            with g = (+1) . sum, f = (+), v = 1; so using the universal
            property we can write
                ((+1) . sum) [] = 1
                ((+1) . sum) (x:xs) = fold (+) x (((+1) . sum) xs)
                
             Which can be written more succintly as
                sum []     + 1 = 1
                sum (x:xs) + 1 = x + (sum xs + 1)
-}

{-
    THE FUSION PROPERTY OF FOLD
    
        For all finite lists, the composition of an arbitrary function
        and a fold can be fused together to give a single fold.
        
            h w       = v           
            h (g x y) = f x (h y)
            => h . fold g w = fold f v
            
        Example
            (+1) . fold (+) 0 = fold (+1) 1
            
            matches the fusion property, so we can conclude
            
            (+1) 0       = 1
            (+1) (+ x y) = (+) x ((+1) y)
            
            which, simplified, gives
                (x + y) + 1 = x + (y + 1)
                
        We can generalize the above on any associative arbitrary function 
        
            (op a) . fold (op) b = fold (op) (b (op) a)
            
        Consider
            map f . map g = map (f . g)
            
        which, by way of the fusion property, can be written as
            map f = []
            map f (g x:y) = (f . g) x : map f y
                  
-}

{-
    DEFINITION PRINCIPLE
    
    We can use the Universal Property as a Definition Principle that
    guides the transformation of recursive definitions into folds
    
    Example:
    
    Redefining 'sum' as a fold
    
    Recursive sum:
        sum :: [Int] -> Int
        sum []     = 0
        sum (x:xs) = x + sum xs
        
    We want to convert it to
        sum = fold f v
        
    And we know 'fold f v' is equivalent to the two equations
        sum []     = v
        sum (x:xs) = f x (sum xs)
    
    From the first equation and the recursive equation, it's easy to 
    see that v = 0
    
    From the second equation, we can calculate a defintion for f
           sum (x:xs) = f x (sum xs)
        => x + sum xs = f x (sum xs)        from def of sum
        => x + y      = f x y               generalizing (sum xs) to y
        => f = (+)                          functions
        
    so sum = fold f v = fold (+) 0

    An example going from a recursive map to a fold
        map :: (a -> b) -> ([a] -> [b])
        map f []     = []
        map f (x:xs) = f x : map f xs
    
    To transform this to a fold we need to solve for g and v in
        map f = fold g v
    
    From the Universal Property, we know the above is equivalent
    to the following two equations
    
        map f []     = [] 
        map f (x:xs) = g x (map f xs)
        
    From the first equation, it's easy to see v = []
    From the second equation, we calculate the equation for g
    
           map f (x:xs) = g x (map f xs)
        => f x : map f xs = g x (map f xs)      def of map
        => f x : ys = g x ys                    generalize map to ys
        => g = \x ys -> f x : ys                functions
        
        [g is a function that takes two params (x, ys) and returns
         f x : ys  .... that's the conversion on the last line]
         
    So map f = (\x ys -> f x:ys) []
    
-}








