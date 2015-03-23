module TFWH07E where

import Data.List (foldl')

{-
    Chapter 7 - Efficiency - Notes and Exercises
    
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/

-}
-- 7.1 Lazy Evaluation
--
-- two definitions that return all the subsequences of a list
subseqs1, subseqs2 :: [a] -> [[a]]
-- xs will be evaluated twice
subseqs1 (x:xs) = subseqs1 xs ++ map (x:) (subseqs1 xs)

-- xs will only be evaluated once but has another problem,
-- xs will be retained in memory until the function exits
-- because it is being referenced by xss
subseqs2 (x:xs) = xss ++ map (x:) xss
    where xss = subseqs2 xs
    
-- so, subseqs1 will take longer as requires two evaluations
-- subseqs1 will be faster but it may rapidly run out space (memory)    

-- related examples
foo1 n = sum (take n primes)
    where
        primes     = [ x | x <- [2..] , divisors x == [x] ]
        divisors x = [ d | d <- [2..x], x `mod` d == 0 ]
        
foo2 n = sum (take n primes)
primes     = [ x | x <- [2..] , divisors x == [x] ]
divisors x = [ d | d <- [2..x], x `mod` d == 0 ]

{-
    foo1 and foo2 have exactly the same definitions EXCEPT
    foo2 defined 'primes' and 'divisors' as global definitions
    while foo1 makes them local to itself.
    
    Following is an example of the time difference in executing
    the two functions.
    
    First, foo1 
    
        *TFWH07E> :set +s
        *TFWH07E> foo1 1000
        3682913
        (5.66 secs, 1042418784 bytes)
        *TFWH07E> foo1 1000
        3682913
        (5.66 secs, 1057044880 bytes)
        *TFWH07E>     
        
        Note that the result (3682913) and time (5.66 secs) are
        the same and the number of bytes used is roughly the same 
        for two identical executions of foo1
        
    Now, 2 executions using foo2
    
        *TFWH07E> foo2 1000
        3682913
        (5.56 secs, 1050721656 bytes)
        *TFWH07E> foo2 1000
        3682913
        (0.00 secs, 0 bytes)
        *TFWH07E> 
        
        We get the same result for both executions, and the time
        and bytes used for the first execution are almost the same
        as the foo1 usage BUT look at the second execution! The
        same result but in 0 seconds and using zero bytes! 
        
        'primes' in the first foo2 execution evaluates the first
        1000 primes AND POINTS TO THEM IN MEMORY. On the second
        execution, since primes is already pointing to its previously
        value, it just returns the same.
    
-}
-- a third approach, the same as foo1 but using a lambda at the
-- function level
foo3 = \n -> sum (take n primes)  -- equiv to: sum . flip take primes
    where
        primes     = [ x | x <- [2..] , divisors x == [x] ]
        divisors x = [ d | d <- [2..x], x `mod` d == 0 ]

{-
    Looking at execution times:
    
        *TFWH07E> foo3 1000
        3682913
        (5.61 secs, 1057142616 bytes)
        *TFWH07E> foo3 1000
        3682913
        (0.00 secs, 0 bytes)
        *TFWH07E> 

        The result/time/space is the same as the foo2 execution. Why?
        
        foo1 is bound to the APPLICATION of foo1, the application
             is dependent on the argument 'n'
             
        foo3 is bound to the lambda FUNCTION, there is NO dependence
             on the argument and so foo3, once run, can continue to
             point to the generated value for the first 1000 primes
             (actually, to a list of the first 1000 primes plus a
              function defining how the rest are to be calculated)
-}
    
-- 7.2 Controlling Space

{-
    foldl can take up a good deal of memory as the entire list
    is evaluated before the accumulator is evaluated; 
    foldl' gets around this by using a combination of eager
    and lazy evaluation (the accumulator is reduced at each step
    vs only once the end of the list has been reached) 
    ie the evaluation of foldl' 'proceeds in constant space'
    
    An instructive example: 
        Taking the mean (average) of a list
    
        an initial attempt may be
        
            mean :: [Float] -> Float
            mean xs = sum xs / length xs
        
        This definition has a number of problems:
        
        1. The type signature for length is [a] -> Int
           and we cannot divide a Float by an Int without
           an explicit conversion
           
           Possible fix:
                mean xs = sum xs / fromIntegral (length xs)
           
        2. The list may be empty and so have no length,
           we cannot divide by zero
           
           Possible fix:
                mean [] = 0
                mean xs = sum xs / fromIntegral (length xs)
                
        3. Now the real problem with mean appears; it has a 
           space leak. Computing mean [1..1000] will cause
           the list [1..1000] to remain in memory after summing
           because there is a second pointer to it in the
           computation of its length.
           
           We can replace the two list traversals by using a
           strategy for optimization called 'tupling' 
          
            sumlen :: [Float] -> (Float, Int)
            sumlen = foldl' g (0,0) 
                where g (s,n) x = (s+x,n+1)
            
          And re-writing our defintion to use the helper function
            mean [] = 0
            mean xs = s / fromIntegral n
                where (s,n) = sumlen xs
                
        4.  But, as it turns out, we still have a space problem
            hidden within the helper function. If we expand
            foldl' we find
            
                foldl' f (s,n) (x:xs) = y `seq` foldl' f y xs
                    where y = (s+x,n+1)
                    
            'seq' is a primitive function that evalates x and
            then returns the result of evaluating y. If x does
            not terminate then x `seq` y will not terminate.
                    
            It turns out the two component is (s+x,n+1) will NOT
            be evaluated until the end of the list is reached so
            we have to re-write our helper as:
            
                sumlen = foldl' f (0,0)
                    where f (s,n) x = s `seq` n `seq` (s+x,n+1)
         
        Now everything should work ok         

-}

mean :: [Float] -> Float
mean [] = 0
mean xs = s / fromIntegral n
    where (s,n) = sumlen xs
    
sumlen :: [Float] -> (Float,Int)
sumlen = foldl' f (0,0)
    where f (s,n) x = s `seq` n `seq` (s+x,n+1)
    
{-
    Two application operators
    -------------------------
    ($) and ($!) defined as:
    
        infixr 0 $, $!          -- have the lowest binding
        ($),($!) :: (a -> b) -> a -> b
        f $ x  = f x
        f $! x = x `seq` f x
        
        ($!) forces the evaluation of x before applying f
        both associate to the right in expressions
        
    Allows us to write:
        process 1 $ process2 $ process3 input
        
    instead of:
        process1 (process2 (process3)) 
                 (process1 . process2 . process3) x

-}    
--  7.3 Controlling Time
{-
    We need to understand things that can slow down execution
    time. The GHC manual gives the following 3 great pieces of advice:
    
    1. Make use of profiling tools to find out where the time
       and space is being used up.
       
    2. The best way to improve performance is to use a better
       algorithm.
       
    3. Don't roll your own code unless you have to; much better
       to use library functions that have been tuned by others
       
    It also gives a number of tips, two of which are:
    
    1. If you know that a functions value will always be needed,
       use eager evaluation: 'Strict functions are your dear friends'
       
    2. Tailor the types of your functions to what you really need
-}
{-
    Sometimes list comprehensions can hide efficiency problems
    Consider the following cross-product defintions
     
-}
cp :: Num a => [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss]

cp' :: Num a => [[a]] -> [[a]]
cp' = foldr op [[]]
    where op xs yss = [ x:ys | x <- xs, ys <- yss ]
    
{-
    They look the same but see what can happen
    
        *TFWH07E> sum $ map sum $ cp [[1..10] | j <- [1..6]]
        33000000
        (6.72 secs, 1612860200 bytes)
        *TFWH07E> sum $ map sum $ cp' [[1..10] | j <- [1..6]]
        33000000
        (2.28 secs, 684912248 bytes)
        *TFWH07E>     

    The first evaluation takes 3 times as long as the second.
    To see why, we need to see what happens to the first
    definition if we eliminate the list comprehension
-}    
cp'' :: Num a => [[a]] -> [[a]]
cp'' [] = [[]]
cp'' (xs:xss) = concat (map f xs)
    where f x = [ x:ys | ys <- cp xss ]
    
{-
    we can see that cp xss is evaluated EVERY TIME f is applied!
    so it is being evaluated many more times in cp than it is in
    cp'.
    
    We could also have written it, efficiently, as
-}    
cp1 :: Num a => [[a]] -> [[a]]
cp1 [] = [[]]
cp1 (xs:xss) = [ x:ys | x <- xs, ys <- yss]
    where yss = cp1 xss
    
{-
    Comparing evaluation times:
    
        *TFWH07E> sum $ map sum $ cp [[1..10] | j <- [1..6]]
        33000000
        (6.59 secs, 1615034744 bytes)
        *TFWH07E> sum $ map sum $ cp'' [[1..10] | j <- [1..6]]
        33000000
        (6.78 secs, 1517104056 bytes)
        *TFWH07E> 

        *TFWH07E> sum $ map sum $ cp' [[1..10] | j <- [1..6]]
        33000000
        (2.31 secs, 855558648 bytes)
        *TFWH07E> sum $ map sum $ cp1 [[1..10] | j <- [1..6]]
        33000000
        (2.19 secs, 708507944 bytes)
        *TFWH07E>     
        
        [Note: It's not the list comprehension per se that is causing
               the problem but the way it is implemented.]
    
-}
-- The rest of the chapter has to do with time analysis, with comparisons
-- of various sort strategies; GHC profiling is mentioned but no usage
-- examples are given
--    
-- for info on profiling, see Chapter 5 of the GHC documentation;
-- there are also a number of packages dedicated to profiling