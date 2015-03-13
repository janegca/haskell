module W06 where

import Data.Array

{-
    Week 06 - Lazy Evaluation
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        06-laziness.html
-}    

{-
    Strict evaluation
    -----------------
        arguments are fully evaluated before being passed to the function
        makes it easy to predict when, and it what order, things will
        happen
        
    Side Effects and purity
    -----------------------
    side effect - anything that causes evaluation of an expression to
                  interact with anything outside itself
                  i.e. modifying a global variable
                       print to the screen
                       reading/writing from/to a file
                       
    these events are 'time-sensitive'; the order they happen in matters
    
    Lazy Evaluation
    ---------------
        evaluation of arguments is delayed as long as possible
        arguments are packed up as 'unevaluated expressions' (thunks)
        
    Pattern Matching drives evaluation
    ----------------------------------
    
    Two examples:
    
            f1 :: Maybe a -> [Maybe a]
            f1 m = [m,m]
        
        here, f1 does not need to know anything about 'm', it can
        just add it a list

            f2 :: Maybe a -> [a]
            f2 Nothing  = []
            f2 (Just x) = [x]
            
        f2, on the other hand, NEEDS to know something about the
        argument before it can continue; the result of 'f2 e' 
        depends on the shape of 'e'
        
        'thunks' are evaluated ONLY enough to allow a pattern
        match to succeed (or fail) and no more
        eg     f2 (safeHead [3^500, 49])
            -> (Just 3^500)
            
        the (3^500) would not be evaluated at this point as it
        is not needed; the pattern (Just x) has been matched.
        
        Hence, "pattern matching drives evaluation"
            - expressions are only evaluated when they are pattern 
              matched AND only in so far as need to match the
              expression
              
        Example:
        
                take 3 (repeat 7)
                
            Definitions of the functions:
            
                repeat :: a -> [a]
                repeat x = x : repeat x

                take :: Int -> [a] -> [a]
                take n _      | n <= 0 =  []
                take _ []              =  []
                take n (x:xs)          =  x : take (n-1) xs  
            
            Walkthrough:
            
                take 3 (repeat 7)
             -> take 3 (repeat 7)               
             -> take 3 (7 : repeat 7)           
             -> 7 : take (3-1) (repeat 7)       
             -> 7 : take 2 (repeat 7)
             -> 7 : take 2 (7 : repeat 7)
             -> 7 : 7 : take (2-1) (repeat 7)
             -> 7 : 7 : take 1 (repeat 7)
             -> 7 : 7 : take 1 (7 : repeat 7)
             -> 7 : 7 : 7 : take (1-1) (repeat 7)
             -> 7 : 7 : 7 : take 0 (repeat 7)
             -> 7 : 7 : 7 : []
             
            [Note: this is a rough approx., GHC will optimize were it can]
-}
{-
    Consequences of lazy evaluation
    -------------------------------
        - requires purity (no side-effects)
        - makes it difficult, sometimes, to reason about the space
          your program requires
          
        i.e. consider: foldl (+) 0 [1,2,3]
        
            -- Standard library function foldl, provided for reference
            foldl :: (b -> a -> b) -> b -> [a] -> b
            foldl _ z []     = z
            foldl f z (x:xs) = foldl f (f z x) xs
            
            Walkthrough:
            
                foldl (+) 0 [1,2,3]
             -> foldl (+) (0 + 1) [2,3]
             -> foldl (+) ((0 + 1) + 2) [3]
             -> foldl (+) (((0 + 1) + 2) + 3) []
             -> (((0 + 1) + 2) + 3)
             -> ((1 + 2) + 3)
             -> (3 + 3)
             -> 6
             
        Because the value of the accumulator is not required until
        the entire list has been recursed, it builds up in memory.
        With long lists this can become a major problem. It's also
        inefficient, we are building a second list to little purpose.
        
        The solution is foldl'.
        
        foldl'           :: (b -> a -> b) -> b -> [a] -> b
        foldl' f z0 xs0 = lgo z0 xs0
            where lgo z []     = z
                  lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs 

        foldl' forces an evaluation of the accumulator through the
               use of the 'seq' operator which takes two arguments
               of any type and returns the second BUT always forces
               evaluation of the first argument.
               
        So, we get
                foldl' (+) 0 [1,2,3]
             -> foldl' (+) (0 + 1) [2,3]
             -> foldl' (+) 1 [2,3]
             -> foldl' (+) (1 + 2) [3]
             -> foldl' (+) 3 [3]
             -> foldl' (+) (3 + 3) []
             -> foldl' (+) 6 []
             -> 6
        
        In the above, laziness got in the way and we had to make
        our program less lazy.
        
    Short-circuiting operators
        - automatic with laziness as operators are only evaluated
          as needed but still have to plan our functions to 
          consider the effects of laziness
          
        i.e. (&&) is defined in the Prelude as

                (&&) :: Bool -> Bool -> Bool
                True  && x = x
                False && _ = False
                
            If the first argument is False the second argument is
            never evaluated; however, what if the function was
            written as follows:
            
                (&&!) :: Bool -> Bool -> Bool
                True  &&! True  = True
                True  &&! False = False
                False &&! True  = False
                False &&! False = False
                
            Both arguments would ALWAYS be evaluated making the time
            required to evaluate something like
            
                False &&! (34^9784346 > 34987345)
                
            unnecessarily long while something like
            
                False &&! (head [] == 'x')
                
            will crash.
                
        Compare the two
-}
(&&!) :: Bool -> Bool -> Bool
True  &&! True  = True
True  &&! False = False
False &&! True  = False
False &&! False = False

ex1 = False && (34^9784346 > 34987345)
ex2 = False &&! (34^9784346 > 34987345)

ex3 = False &&  (head [] == 'x')
ex4 = False &&! (head [] == 'x')

{-
    User defined control structures
        - we can define our own control structures; don't need
          to rely solely on built-in if/while/do etc structures
          
    Infinite data structures - are possible as only the elements
        of the structure that are explored are evaluated
        
    Pipelining (wholemeal programming) - incremental transformations
        by pipelining can be memory efficient as only the results
        needed by the next step are produced at any one time
        
    Dynamic programming - requires that all entries of a dynamic
        programming table be filled in in order; with Haskell,
        lazy evaluation lets us use the runtime to figure out the
        order
        
    eg below is code to solve the 0-1 knapsack problem which wikipedia
       describes as:
       
        "The knapsack problem or rucksack problem is a problem in 
        combinatorial optimization: Given a set of items, each with 
        a mass and a value, determine the number of each item to include 
        in a collection so that the total weight is less than or equal 
        to a given limit and the total value is as large as possible. 
        It derives its name from the problem faced by someone who is 
        constrained by a fixed-size knapsack and must fill it with the 
        most valuable items....The most common problem being solved is 
        the 0-1 knapsack problem, which restricts the number xi of copies 
        of each kind of item to zero or one."
        
        Basic formulation of the problem (also from wikipedial):
        
            Let there be n items, z_1 to z_n, where z_i has value v_i
            and weight w_i.
            x_i is the number of copies of z_i, which must be 0 or 1
            W is the maximum weight we can carry in the bag
            Assume all values and weight are non-negative
            
            Maximize the sum of the values of the items in the knapsack so 
            that the sum of the weights must be less than or equal to 
            the knapsack's capacity.
            
        [Notes:  (!) is defined in Data.Array to return the element
                     in an immutable array at the specified index]
                      
-}
knapsack01 :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20