{-
    Chapter 1 - Getting Started
    
    Notes:
        variables - "denote arbitrary elements of the type over which
                       they range"
        
        functions - "operations on data-structures"
        
        constructors - "the building blocks of data structures"
        
        =   in Haskell read "is by definition equal to"
        
    Types   - always capitalized
        Int         - fixed precision integers
        Integers    - arbitrary precision integers (allocated space
                      depends on the size of the object)
                      
    type scheme - a type signature with a constraint
                  e.g. fn :: Integral a => a -> a -> a
                 
                  or using a 'type variable' rather than a concrete
                  type i.e. [Int] is a 'type', [a] is a 'type scheme'
                                                 
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
module GS where

--
-- Determines if a number, 'd', evenly divides another number, 'n'
--
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

--
-- Return the least divisor of a number, n 
-- [Note: the least divisor of a number will always be less than the
--        square root of the number, as a number is always divisible
--        by its square root; hence, if k^2 > n, it is greater then
--        the square root of n and cannot be the least divisor.]
--
ld :: Integer -> Integer
ld n = ldf 2 n
    where
        ldf :: Integer -> Integer -> Integer
        ldf k n | divides k n = k
                | k^2 > n     = n
                | otherwise   = ldf (k+1) n
        
--
-- First attempt at checking if a number is a prime
-- [Note: the least divisor of n is always a prime]
--
prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False        -- the number 1 is not a prime #
         | otherwise = ld n == n    -- if True, 'n' is a prime #

--
-- Returns the minimum value from a list of integers
--
mnmInt :: [Int] -> Int
mnmInt []     = error "empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min x (mnmInt xs)   

min' :: Int -> Int -> Int           -- a version of built-in min
min' x y | x <= y    = x
         | otherwise = y

intMinBound = minBound :: Int       -- bounds of the type Int
intMaxBound = maxBound :: Int
     
--
-- Exercise 1.9
--  Return the maximum value from a list of integers
--
maxInt :: [Int] -> Int
maxInt []       = error "empty list"
maxInt [x]      = x
maxInt (x:xs)   = max x (maxInt xs)

--
-- Exercise 1.10
--  Remove the first occurrence of a number from a list of integers
--  If the number is not in the list, the list is returned unchanged
--
removeFst :: Int -> [Int] -> [Int]
removeFst m []     = []
removeFst m (x:xs) | m == x    = xs
                   | otherwise = x : (removeFst m xs)
                   
--
-- Sort a list of integers in ascending order
--
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs))
     where
        m = mnmInt xs
        
srtInts' :: [Int] -> [Int]          -- example using 'let'
srtInts' [] = []
srtInts' xs = let m = mnmInt xs
              in m : (srtInts' (removeFst m xs))        
        
--
-- Return the average of a list of integers
--
average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum xs) / toRational (length xs)

sum' :: [Int] -> Int                -- a version of the built-in sum
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

length' :: [a] -> Int             -- a version of the built-in length
length' []     = 0
length' (x:xs) = 1 + length' xs   

--
-- Exercise 1.13
--      Count the number of times a character appears in a string.
--
count :: Char -> String -> Int
count c []  = 0
count c (x:xs) | c == x    = 1 + count c xs
               | otherwise = count c xs
               
--
-- Exercise 1.14
--    Converts a string in the form of a1a2a3... to a string
--    in the form a1a2a2a3a3a3...
--
blowup :: String -> String
blowup []   = []
blowup str  = bu 1 str
    where    
        bu :: Int -> String -> String
        bu n []     = []
        bu n (x:xs) = repeatChr x n  ++ bu (n+1) xs
        
        repeatChr :: Char -> Int -> String
        repeatChr c 0  = []
        repeatChr c n  = c : repeatChr c (n-1)
    
-- alternate solution
spread :: [a] -> [a]
spread xs = [ x | (n,y) <- zip [1..] xs , x <- take n (repeat y)]    

--
-- Exercise 1.15
--      Sort a list of strings in alphabetical order
--
srtString :: [String] -> [String]
srtString []  = []
srtString xss = m : (srtString (removeFst' m xss))
    where
        m = mnm xss
        
        mnm []     = error "empty list"        
        mnm [x]    = x
        mnm (x:xs) = min x (mnm xs)

        removeFst' m []     = []
        removeFst' m (x:xs) | m == x    = xs
                            | otherwise = x : (removeFst' m xs)

-- NOTE:  solution text advises generalizing the minInt, 
--        at srtInt so functions can be used on any type
--        (would also mean generalizing 'removeFst')
mnm' :: Ord a => [a] -> a
mnm' []     = error "empty list"
mnm' [x]    = x
mnm' (x:xs) = min x (mnm' xs)

removeFst'' :: Ord a => a -> [a] -> [a]
removeFst'' m []     = []
removeFst'' m (x:xs) | m == x = xs
                     | otherwise = x : (removeFst'' m xs)
                     
srt :: Ord a => [a] -> [a]
srt [] = []
srt xs = m : (srt (removeFst'' m xs)) where m = mnm' xs
                            
--
-- Return True if string a is a prefix of string b
--
prefix :: String -> String -> Bool
prefix [] ys         = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys 

--
-- Exercise 1.17
--      Return True if string a is a substring of string b
--
substring :: String -> String -> Bool
substring xs []         = False
substring xs ys@(y:ys') | prefix xs ys  = True
                        | prefix xs ys' = True 
                        | otherwise     = substring xs ys'
                       
--
-- Exercise 1.18
--      Find expressions for the given types
--
ex18a = ["hello","goodbye"]
ex18b = (True, "true")
ex18c = [(False, "false")]
ex18d = ([True,False,False], "booleans")
ex18e x = f x where f x = not x
        
--
-- Exercise 1.19
--      use the interpreter command :t to find the types of the
--      following:
--  
--      head        [a] -> a
--      last        [a] -> a
--      init        [a] -> [a]
--      fst         (a,b) -> a
--      (++)        [a] -> [a] -> [a]
--      flip        (a -> b -> c) b -> a -> c
--      flip (++)   [a] -> [a] -> [a]
--
--      Now try the functions out with arguments of the expected types
--      and try to guess what they do
ex19a = head [1..20]
ex19b = last [1..20]
ex19c = init [1..20]
ex19d = fst ('a','b')
ex19e = (++) "abc" "def"
ex19f = flip (++) ['a','b'] ['c','d']

-- 
-- Return the prime factors of a given integer, n
--
factors :: Integer -> [Integer]
factors n | n < 1 = error "argument not positive"
          | n == 1 = []
          | otherwise = p : factors (div n p) where p = ld n
          
--
-- a version of map
--
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map f xs    
      
--
-- Exercise 1.20
--      Using map, write a function, lengths, that takes a list
--      of lists ans returns the list of the corresponding list lengths
--
lengths :: [[a]] -> [Int]
lengths xs = map length xs

--
-- Exercise 1.21
--      Write a function, sumLengths, that takes a list of lists
--      and returns the sum of their lengths
--
sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)

-- alternative, using point free style
sumLengths' :: [[a]] -> Int
sumLengths' = sum . lengths

--
-- A version of filter
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x       = x : filter' p xs
                 | otherwise = filter' p xs
                 
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

--
-- Haskell uses 'equational reasoning'
--
a = 3
b = 4

f :: Integer -> Integer -> Integer
f x y = x^2 + y^2

exEr = f a (f a b)
                