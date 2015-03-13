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
    10.2 Processes and Labelled Transition Systems
    
    Processes are interacting procedures; typical examples are 
    models of mechanical devices: clocks, traffic control protocols,
    vending machines, operating systems, client-server systems, etc.
    
    Processes can be modeled using a 'labelled transition system'
    (QAT) which consists of 'states',Q, a set of 'action labels', A,
    and the 'transition relation'.
    
    A simple model is a system given by the modle of a clock that
    ticks; it has two states, c and c_0, ande two transitions
    c -- tick -- c_0 and c -- crack -- c_0.  The process is
    nondeterministic; the clock keeps ticking until, for no
    reason, it gets stuck.
    
    "Nondeterministic behaviour is behavior determined by random
     factors,so a simple way of modeling nondeterminism is by
     modeling a process as a map from a randomly generated list
     of integers to a stream of actions."
     
    Haskell provides the module Random.hs, with functions mkStdGen
    and randomRs that can be used to generate random numbers.

-}
{-
    Exercise 10.5
    
    Note that randomInts 1 seed generates a random stream of
    0's and 1's with the proportion 1 to 1. How would you 
    generate a stream with a proportion of 0's and 1's at 2 to 1?
-}
randomInts :: Int -> Int -> [Int]
randomInts bound seed = tail (randomRs (0,bound) (mkStdGen seed))

-- provided solution
random001s :: Int -> [Int]
random001s i = map (`mod` 2) (randomInts 2 i)

ex105a = take 20 (randomInts 1 17)
ex105b = take 20 (random001s 17)

{-
    A process is defined as a map from streams of integers to
    streams of action labels; to begin a process, generate
    an appropriate stream of integers and feed it to the process.

    
-}
type Process = [Int] -> [String]

start :: Process -> Int -> Int -> [String]
start process bound seed = process (randomInts bound seed)

clock :: Process
clock (0:xs) = "tick"  : clock xs
clock (1:xs) = "crack" : []

-- [Note: matching bound, seed values produce matching results.
--        these results are the same as those given in the book.]
ex102a = start clock 1 1    -- ["tick","crack"]
ex102b = start clock 1 2    -- ["crack"]
ex102c = start clock 1 25   -- ["tick","tick","tick","tick","crack"]

{-
    Example 10.6
    
    A vending machine with a coin slot and a dispense button.
    The machine takes 1 euro coins.
    If 1 coin is inserted and the button is pushed, a can of 
    mineral water is dispensed.
    If 2 coins are inserted and the button is pushed, a can of
    beer is dispensed.
    If a 3rd coin is inserted, all 3 coins are returned.
    In all 4 states are needed:
    
        q   -- coin  -- q_1  q_1 -- water -- q
        q_1 -- coin  -- q_2  q_2 -- beer  -- q
        q_2 -- coin  -- q_3  q_3 -- money -- q
        
    A random stream is not needed for the transitions q->q_1 and
    q_3->q because the only possible actions are inserting a coin
    for the first, and returning all coins for the second.

    [Note: corecursion spans the 4 states]
-}
vending, vending1, vending2, vending3 :: Process
vending  (0:xs) = "coin"    : vending1 xs
vending  (1:xs) = vending xs

vending1 (0:xs) = "coin"    : vending2 xs
vending1 (1:xs) = "water"   : vending  xs

vending2 (0:xs) = "coin"    : vending3 xs
vending2 (1:xs) = "beer"    : vending  xs

vending3 (0:xs) = "moneyback" : vending xs
vending3 (1:xs)  = vending3 xs

ex106a = take 9 (start vending 1 1)
ex106b = take 8 (start vending 1 3)
ex106c = take 8 (start vending 1 22)

{-
    Example 10.7
    
    A parking ticket dispenser has a coin slot that takes 1 or 2
    euro coins, a red button that returns all inserted coins, and
    a green button that prints the parking ticket.
    
    For each 1 euro value, parking time is increased 20 minutes.
    
    The actions give the following states and transitions:
    
        q(i) -- return (i) -- q(0)
        q(i) -- 1 euro     -- q(i+1)
        q(i) -- 2 euro     -- q(i+2)
        q(0) -- no time    -- q(0)
        q(i) -- time i*20  -- q(0)
        
    Note that the number of states is infinite.

-}
ptd :: Process
ptd = ptd0 0

ptd0 :: Int -> Process
ptd0 0 (0:xs) = ptd0 0 xs
ptd0 i (0:xs) = ("return " ++ show i ++ " euro") : ptd0 0 xs
ptd0 i (1:xs) = "1 euro" : ptd0 (i+1) xs
ptd0 i (2:xs) = "2 euro" : ptd0 (i+2) xs
ptd0 0 (3:xs) = ptd0 0 xs
ptd0 i (3:xs) = ("ticket " ++ show (i * 20) ++ " min") : ptd0 0 xs

ex107 = take 6 (start ptd 3 457)

{-
    Exercise 10.9
    
    Consider the vending machine given by the following transitions:
    
        q   --- coin --- q_1
        q   --- coin --- q_4
        q_1 --- coin --- q_2
        q_2 --- beer --- q
        q_2 --- coin --- q_3
        q_3 --- money -- q
        q_4 --- water -- q
        
    If this example and the machine in Example 10.6 are black boxes,
    how can a user find out which of the two machines they are
    operating?
    
    Ans:  this machine will not always deliver water after inserting
          one coin while the example 10.6 machine will (no transition
          from q_1 to q)
-}
{-
    Exercise 10.10
    
    Give a Haskell implementation of the vending machine from
    exercise 10.9

-}
vm, vm1, vm2, vm3, vm4 :: Process
vm  (0:xs) = "coin"     : vm1 xs
vm  (1:xs) = "coin"     : vm4 xs

vm1 (0:xs) = "coin"     : vm2 xs
vm1 (1:xs) = vm1  xs

vm2 (0:xs) = "beer"     : vm  xs
vm2 (1:xs) = "coin"     : vm3 xs

vm3 (0:xs) = "money"    : vm  xs
vm3 (1:xs) = vm3 xs

vm4 (0:xs) = "water"    : vm xs
vm4 (1:xs) = vm4 xs

ex1010a = take 9 (start vm 1 1)
ex1010b = take 8 (start vm 1 3)
ex1010c = take 8 (start vm 1 22)

{-
    Example 10.11
    
    Modelling a beer drinker who interacts with a vending machine.
    We start with the user buying their first beer.
    Next, we feed the stream of actions produced by the vending
    machine as input to the user, and the stream of actions
    produced by the user as input to the vending machine.
    The two keep each other busy.
    
    [Note:
        The tilde operator (~) marks the following pattern
        as a 'lazy pattern'; they always match (at compile time).
        In this instance, it allows the initial request to be
        submitted before it actually comes into existence.
        
        The lazy pattern is not checked until the values are 
        actually used; this can result in  runtime errors if the 
        passed arguments do not match the pattern]
-}
actions   = user [0,0,1] responses
    where
        user acts ~(r:s:p:resps)   = acts ++ user (prc [r,s,p]) resps
        prc ["coin","coin","beer"] = [0,0,1]
    
responses = vending actions

ex1011a = take 8 actions
ex1011b = take 8 responses











