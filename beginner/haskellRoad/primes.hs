-- primes

--
-- Return the least divisor of n (if n is prime, returns n)
--
ldp :: Integer -> Integer
ldp = ldpf primes1
    where
        ldpf :: [Integer] -> Integer -> Integer
        ldpf (p:ps) n | rem n p == 0 = p
                      | p^2 > n      = n
                      | otherwise    = ldpf ps n
                      
        primes1 :: [Integer]
        primes1 = 2 : filter prime [3..]        -- infinite list of primes

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n  

-- Mersenne numbers - prime numbers of the form 2^n + 1        
-- return primes in the range 2^n + 1  (big gap after the 6th one)
ex1 = take 6 [ 2^n + 1 | n <- [0..], prime (2^n + 1) ]

-- return primes not in the range 2^n + 1
ex2 = take 30 [ 2^n + 1 | n <- [0..], not (prime (2^n + 1)) ]


        