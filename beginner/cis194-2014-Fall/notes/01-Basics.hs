{- CIS194 - Week 1 Introduction to Haskell
   Source: http://www.seas.upenn.edu/~cis194/lectures/01-intro.html
   
   Suggested Readings:
    'Learn You a Haskell for Great Good' Chapter 2
    'Real World Haskell' Chapters 1 and 2
   
        Declarations and Variables
        Basic Types
        GHCi
        Arithmetic
        Boolean Logic
        Defining Basic Functions
        Pairs
        Using functions and multiple arguments
        Lists
        Constructing Lists
        Functions on Lists
        Combining Functions
        Error Messages
   
-}   
{-
    Variables 'define' a value, they do not 'hold' a value
    i.e. x = 4 should be read as 'x is defined to be 4'
-}

-- range of Int on your machine, limited to
{-
    *Main> biggestInt
    9223372036854775807
    *Main> smallestInt
    -9223372036854775808
    
    which is 2^63 on 64-bit machine
-}
biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

-- range of Integer limited to the amount of memory on the machine
-- Arbitrary-precision integers
n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)

-- Double-precision floating point
d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

-- Booleans
b1, b2 :: Bool
b1 = True
b2 = False

-- Unicode characters
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- Strings are lists of characters with special syntax
s :: String
s = "Hello, Haskell!"

-- evaluating expressions
i = 30 :: Int

ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1        -- floating point division
ex05 = mod 19 3
ex06 = 19 `mod` 3
ex07 = 7 ^ 222
ex08 = (-3) * (-7)

--badArith1 = i / i     -- raises error
ex09 = i `div` i        -- no error, integer division
ex10 = 12 `div` 5       

-- Boolean logic
ex11 = True && False
ex12 = not (False || True)
ex13 = ('a' == 'a')
ex14 = (16 /= 3)                    -- not equal
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"

-- Defining Basic Functions

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- using 'guards'
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1
  
-- a more complex but contrived example
-- What is foo (-3)? foo 0? foo 1? foo 36? foo 38?
foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3  
  
-- abstracting out isEven, this works but is much to complicate
-- Can you see why?  
isEven :: Integer -> Bool
isEven n 
  | n `mod` 2 == 0 = True
  | otherwise      = False  
  
-- the result of the 'mod' is itself a Boolean so just use it
isEven' :: Integer -> Bool
isEven' n = (n `mod` 2) == 0

-- Pairs - elements can be extracted with pattern matching
-- Haskell has triples, quadruples, etc but advises agains using them
p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y

-- Functions with multiple arguments
-- note the type definition is read as:
--      Arg1Type -> Arg2Type -> ... -> ResultType
-- Reason for this syntax will become clear later
f :: Int -> Int -> Int -> Int
f x y z = x + y + z

ex17 = f 3 17 8

-- 'function application' (fn followed by space and arg) has
-- highest precedence
-- i.e.  f 3 n+1 7 => (f 3 n) + (1 7) NOT f 3 (n+1) 7

-- Lists - most basic data structure
nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]

-- Strings are just lists of characters
-- hello1 and hello2 are exactly the same.

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

-- Constructing Lists - the 'cons' operator (:)
-- [2,3,4] is shorthand for 2 : 3 : 4 : []

emptyList = []

ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []

ex21 = [2,3,4] == 2 : 3 : 4 : []

-- Generate the sequence of hailstone iterations from a starting number.
-- Stops when n == 1
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

-- Using nested patterns
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

-- Combining Functions

-- The number of hailstone steps needed to reach 1 from a starting
-- number.
-- Because of lazy evaluation, the list length and hailstone list
-- generation are interleaved so the list is read once, not twicea
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1


