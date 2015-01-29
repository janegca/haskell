{-
    Chapter 3.7 - Reasoning and Computation with Primes
    
        Using the computer to investigate the theory of primes
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012    

-}

prime :: Integer -> Bool 
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False 
        | otherwise = ldp n == n 
  where 
      ldp           = ldpf primes
      
      ldpf (p:ps) m | rem m p == 0 = p
                    | p^2 > m      = m
                    | otherwise    = ldpf ps m
                    
      primes = 2 : filter prime [3..]

{-
    All primes > 2 are odd so you often see the form 4n+1 or 4n+3
    in connection with finding prime numbers. To show that there
    are infinitely many prime numbers you can use
    
        filter prime [4*n + 3 | n <- [0..]]
        
    Running this will produce an endless amount of numbers and
    an eventual stackoverflow

-}  
ex1 = take 100 (filter prime [4*n + 3 | n <- [0..]])

{-
    Sieve of Eratosthenes
    
        Start with a list of natural numbers
        Mark 2 as prime and remove all multiples of 2 from the list
        Mark 3 as prime and remove all multiples of 3 from the list
        Mark 5 as prime and remove all multiples of 5 from the list
        Mark 7 as prime ...
        etc
        
        The only calculation involved is 'counting' by the starting
        starting prime i.e. to remove all numbers divisible by 3,
        start from 3 and count 1, 2, 3 to arrive at 6, 1,2,3, to
        arrive at 9, etc. Same for counting from 5, count 1,2,3,4,5
        to arrive at 10, count 1,2,3,4,5 to arrive at 15, etc.
        
        In the function below, Haskell's lazy evaluation is leverged
        Non-primes are 'removed' by reducing the to zero
        
-}

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
  where 
  -- simulate the 'counting', each 'n' represents the prime from
  -- which to start the count of 1 to m
  mark :: [Integer] -> Integer -> Integer -> [Integer]
  mark (y:ys) k m | k == m    =  0 : (mark ys  1    m) 
                  | otherwise =  y : (mark ys (k+1) m)

ePrimes :: [Integer]                  
ePrimes = sieve [2..]

ex2 = take 100 ePrimes

-- 
-- Exercise 3.38
--  A faster way to generate primes is to start counting k from any 
--  odd number a = 2n + 1, then move on to number (2n +1) + 2k and 
--  if 'a' is a multiple of k then a + 2k is also a multiple of k
--  Write a function: fasterprimes :: [Integer] using this idea and
--  'oddsFrom3' to generate odd natural numbers starting from 3
--
oddsFrom3 :: [Integer]
oddsFrom3 = 3 : map (+2) oddsFrom3

fasterPrimes :: [Integer]
fasterPrimes = sieve oddsFrom3

ex38 = take 100 fasterPrimes

--
-- Exercise 3.39
--  Write a program to refute the statement that if [p1...pk] is a
--  list of primes < n, then  p1 x p2 x p3 ... x pk) + 1 is a prime.
--
ex39 n = prime $ product (takeWhile (< n) fasterPrimes) + 1

-- provided solution
examples = [ take n fasterPrimes | n <- [0..],
             not (prime (product (take n fasterPrimes) + 1)) ]
     
-- 
-- Mersenne Primes
--      Some numbers 2^n - 1 are prime if 'n' is prime
--
-- both of the following take a very long time, use CTRL+C to break
--     
mersenne    = [ (p,2^p - 1) | p <- fasterPrimes, prime (2^p - 1) ]
notmersenne = [ (p,2^p - 1) | p <- fasterPrimes, not (prime (2^p-1)) ]
             
-- Mersenne Primes are related to 'perfect numbers' (a number is 'perfect'
-- if the sum of its divisors = the number i.e. 1+2+3 = 6)
-- Euclid proved that if 2^n-1 is prime, then 2^(n-1)(2^n-1) is perfect
--
-- get all the divisors of the given number
-- [Note: this is NOT an efficient algorithm]
pdivisors :: Integer -> [Integer]
pdivisors n = [ d | d <- [1..(n-1)], rem n d == 0]
             
isPerfect n = sum (pdivisors n) == n

--
-- Prime Pairs have the form (p, p+2) with both numbers being primes
--
primePairs :: [(Integer, Integer)]
primePairs = pairs fasterPrimes
    where
    pairs (x:y:xs) | x + 2 == y = (x,y) : pairs (y:xs)
                   | otherwise  = pairs (y:xs)
                  
ex41 = take 50 primePairs
                  
                  