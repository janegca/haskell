-- primes

{-
    References:
        [HR]    The Haskell Road to Logic, Math, and Programming
-}

-- Find the divisors of the given natural number [HR]
divs :: Integer -> [Integer]
divs n = [ d | d <- [1..n], rem n d == 0 ]


-- return the prime factors of a number [HR]
factors :: Integer -> [Integer]
factors n | n < 1 = error "argument not positive"
          | n == 1 = []
          | otherwise = p : factors (div n p) 
    where
        p = ld n          

        ld :: Integer -> Integer
        ld n = ldf 2 n
            where
                ldf :: Integer -> Integer -> Integer
                ldf k n | divides k n = k
                        | k^2 > n     = n
                        | otherwise   = ldf (k+1) n
                        
                divides :: Integer -> Integer -> Bool
                divides d n = rem n d == 0
                            
--  A faster way to generate primes is to start counting k from any 
--  odd number a = 2n + 1, then move on to number (2n +1) + 2k and 
--  if 'a' is a multiple of k then a + 2k is also a multiple of k
--  example: take 100 fasterPrimes [HR]
--
fasterPrimes :: [Integer]
fasterPrimes = sieve oddsFrom3
    where
        oddsFrom3 :: [Integer]
        oddsFrom3 = 3 : map (+2) oddsFrom3
        
        sieve :: [Integer] -> [Integer]
        sieve (0 : xs) = sieve xs
        sieve (n : xs) = n : sieve (mark xs 1 n)
          where 
          -- simulate the 'counting', each 'n' represents the prime from
          -- which to start the count of 1 to m
          mark :: [Integer] -> Integer -> Integer -> [Integer]
          mark (y:ys) k m | k == m    =  0 : (mark ys  1    m) 
                          | otherwise =  y : (mark ys (k+1) m)

-- Mersenne Primes [HR]
--      Some numbers 2^n - 1 are prime if 'n' is prime
--
-- Ex: take 5 mersenne
--     
mersenne    = [ (p,2^p - 1) | p <- fasterPrimes, prime (2^p - 1) ]                          
                          
-- a number is perfect if alls its divisors (excepting itself) add
-- to the number i.e. 6 is perfect as 1+2+3=6
-- (this is not an efficient algo)                          
isPerfect n = sum (pdivisors n) == n                          
    where
        pdivisors n = [ d | d <- [1..(n-1)], rem n d == 0]
        
isPerfect' :: Integer -> Bool
isPerfect' n = sum (properDivs n) == n        
    where
        -- all divisors exluding the number itself
        properDivs n = init (divs n)
        
-- check if a number is prime [HR]
isPrime :: Integer -> Bool
isPrime = \n -> divisors n == [(1,n)]
    where
        divisors n = [ (d, quot n d) | d <- [1..k], rem n d == 0 ]
            where k = floor (sqrt (fromInteger n))
        
-- determine if a number is prime [HR]
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
        
             
               
-- Prime Pairs have the form (p, p+2) with both numbers being primes
-- Ex: take 10 primePairs [HR]
primePairs :: [(Integer, Integer)]
primePairs = pairs fasterPrimes
    where
    pairs (x:y:xs) | x + 2 == y = (x,y) : pairs (y:xs)
                   | otherwise  = pairs (y:xs)
                   
