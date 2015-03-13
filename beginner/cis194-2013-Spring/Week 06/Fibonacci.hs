module Fibonacci where

{-
    Week 06 - Lazy Evaluation Homework
    
    This module focuses on one of the consequences of lazy evaluation,
    the ability to work with infinite data structures.

    
    Ref:
        http://www.seas.upenn.edu/~cis194/fall14/spring13/
            hw/06-laziness.pdf
-}

{-
    Fibonacci numbers are defined as the sequence of numbers 
    beginning with 0 and 1 where every integer in the sequence
    is the sum of the previous two.
    
                F_0 = 0
                F_1 = 1
                F_n = F_(n-1) + F_(n-2) n >= 2
    
    For example, the first 15 numbers in the Fibonacci sequence are
            
        0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .
-}
{-
    Exercise 1
    
    Translate the above definition of Fibonacci numbers directly into a
    recursive function definition of type

        fib :: Integer -> Integer

    so that fib n computes the nth Fibonacci number Fn.
    
    Now use fib to define the infinite list of all Fibonacci numbers,
    
        fibs1 :: [Integer]
    
    (Hint: You can write the list of all positive integers as [0..].)
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]
    
{-    
    Try evaluating fibs1 at the ghci prompt. You will probably get
    bored watching it after the first 30 or so Fibonacci numbers, because
    fib is ridiculously slow. Although it is a good way to define the 
    Fibonacci numbers, it is not a very good way to compute them—in order 
    to compute F_n it essentially ends up adding 1 to itself F_n times.
    To compute the 'fib 5' you end with a tree of recursive calls:
                                   
                                    5
                                   / \
                                 4      3
                                / \    / \
                               3   2   2  1
                              / \ / \ / \
                             2  1 1 0 1  0
                            / \
                            1  0
                              
    There is a lot of repeated work. The running time works out to be
    the 'golden ratio: (1 + sqrt(5))/2 i.e. it's exponential in n.
-}
{-

    Exercise 2
   
    Your task for this exercise is to come up with more efficient 
    implementation. Specifically, define the infinite list

        fibs2 :: [Integer]

    so that it has the same elements as fibs1, but computing the first n
    elements of fibs2 requires only O(n) addition operations. B
-}
-- Source: https://github.com/pdswan/cis194/blob/master/hw6/Fibonacci.hs
fibs2 :: [Integer]
fibs2 = 0 : 1 : [ x + y | (x,y) <- zip fibs2 (drop 1 fibs2)]

-- Source: https://github.com/keathley/cis194/src/Cis194/Hw/Fibonacci.hs
fibs2' :: [Integer]
fibs2' = scanl (+) 0 (1:fibs2)

-- Source: Haskell Road
fibs2'' :: [Integer]
fibs2'' = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
        
{-
    We can be more explicit about infinite lists by defining a type Stream
    representing lists that must be infinite. (The usual list type 
    represents lists that may be infinite but may also have some finite 
    length.)
    
    In particular, streams are like lists but with only a “cons” 
    constructor  whereas the list type has two constructors, [] 
    (the empty list) and (:) (cons), there is no such thing as an empty 
    stream. So a stream is simply defined as an element followed by a 
    stream.
-}
{-
    Exercise 3

    • Define a data type of polymorphic streams, Stream.

    • Write a function to convert a Stream to an infinite list,
      
        streamToList :: Stream a -> [a]
   
    • To test your Stream functions in the succeeding exercises, it will 
      be useful to have an instance of Show for Streams. However, if you 
      put deriving Show after your definition of Stream, as one usually 
      does, the resulting instance will try to print an entire Stream—
      which, of course, will never finish. Instead, you should make your 
      own instance of Show for Stream,    
      
            instance Show a => Show (Stream a) where
            show ...
    
      which works by showing only some prefix of a stream (say, the
      first 20 elements).
-}                
data Stream a = Stream a (Stream a)       -- can't use (:) directly

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

{-

    Exercise 4

    Let’s create some simple tools for working with Streams.
    
    • Write a function
            streamRepeat :: a -> Stream a
      which generates a stream containing infinitely many copies of the
      given element.

    • Write a function
            streamMap :: (a -> b) -> Stream a -> Stream b
      which applies a function to every element of a Stream.

    • Write a function
            streamFromSeed :: (a -> a) -> a -> Stream a
            
      which generates a Stream from a “seed” of type a, which is the
      first element of the stream, and an “unfolding rule” of type a -> a
      which specifies how to transform the seed into a new seed, to be
      used for generating the rest of the stream.

-}    
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

ex4a = streamRepeat 1

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

ex4b = streamMap (+2) ex4a

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f (f x)

ex4c = streamFromSeed (+2) 0





