{-
    Chapter 10 - Corecursion
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
module COR where

import System.Random (mkStdGen, randomRs)
import Polynomials
import PowerSeries

-- default display for Num values; normally fractional numbers are
-- displayed as Double's, this will change the default to Rational
-- first and failing that, Double
default (Integer, Rational, Double) 

{-
    Infinite lists, called 'streams', are the most important kind of
    infinite data structures.
    
    We can easily generate infinite lists in Haskell using 'corecursion'
    A corecursive definition is the same as a recursive definition
    EXCEPT it has NO base case.
    
    A corecursive definition salways yield infinite objects
    
    Examples show below
-}

-- use these with 'take n'
ones = 1 : ones             -- infinite list of ones
nats = 0 : map (+1) nats    -- infinite list of natural numbers
odds = 1 : map (+2) odds    -- infinite list of odd numbers

{-
    Exercise 10.1
    
    Write a corecursive definition that generates the even natural
    numbers.
-}
evens = 0 : map (+2) evens

ex101 = take 20 evens

{-
    The 'iterate' function in the Prelude is, itself, corecursive
    We can use it to create infinites lists
-}

theOnes = iterate id   1
theNats = iterate (+1) 0
theOdds = iterate (+2) 1

{-
    Exercise 10.2
    
    Use 'iterate' to define the infinite stream of even natural
    numbers.
-}

theEvens = iterate (+2) 0

ex102    = take 20 theEvens

{-
    We can define the list [0..] corecursively from ones and zipWith.
    if n is a natural number then its successor can be got by 
    adding 1 to n. Zero is the first natural number, the second
    natural number, 1, is gotten from 0 + 1; 2, the third natural
    number is gotten from (0+1)+1
-}

theNats1 = 0 : zipWith (+) ones theNats1

ex101a = take 10 theNats1       -- first 10 Natural numbers

{-
    Fibonacci numbers can be gotten in a similar fashion.
-}

theFibs = 0 : 1 : zipWith (+) theFibs (tail theFibs)

ex101b = take 10 theFibs

{-
    The definition of the Sieve of Eratosthenes also uses
    corecursion
-}
sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
  where 
  mark (y:ys) k m | k == m    =  0 : (mark ys  1    m)
                  | otherwise =  y : (mark ys (k+1) m)

sieve' :: [Integer] -> [Integer]
sieve' (n:xs) = n : sieve' (filter (\ m -> (rem m n) /= 0) xs)

primes :: [Integer]
primes = sieve' [2..]

ex101c = take 10 primes

{-
    Exercise 10.3
    
    The Thue-Morse sequence is a stream of 0's and 1's that is
    produced as follows. First produce 0. Next, at any stage,
    swap everything that was produced so far (by interchanging
    0's and 1's) and append that. The first few few stages
    look like this:
                0
                01
                0110
                01101001
                0110100110010110
                
    Thus, if Ai denotes the first 2^k symbols of the sequence,
    then A_k+1 equals A_k ++ B_k where B_k is obtained from
    A_k by interaging 0's and 1's. Give a corecursive program
    for producing the Thue-Morse sequence as a stream.
-}
-- provided solution
thueMorse :: [Char]
thueMorse = '0' : morse "1"
    where
        morse xs = xs ++ morse(xs ++ swap xs)
        
        swap "" = ""
        swap ('0':xs) = '1' : swap xs
        swap ('1':xs) = '0' : swap xs

ex103 = take 10 thueMorse

{-
    10.2 Processes and Labeled Transition Systems

-}