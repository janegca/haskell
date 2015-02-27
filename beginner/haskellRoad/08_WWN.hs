{-
    Chapter 8 - Working With Numbers
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
import Data.List
import Data.Ratio
import Data.Char
import Data.Complex
import Nats

{-
    The imported 'Nats' module contains all the basic operations
    for working with Natural numbers, including enumeration and
    negation. 
    
    Natural numbers are also in the Real and Integral classes
    which allows for a number of other operations including
    implicit conversion from Integrals to Naturals
-}
exEnum    = enumFromTo Z (toEnum 5) 
exNeg     = negate (S (S (S (S Z))))
exNat12   = 12 :: Natural  
exInt12   = fromIntegral exNat12
exNatExp  = (S (S Z)) ^ (S (S (S Z)))
exNatExpa = fromIntegral exNatExp

{-
    Output:
    
    *Main> exEnum
    [Z,S Z,S (S Z),S (S (S Z)),S (S (S (S Z))),S (S (S (S (S Z))))]
    *Main> exNeg
    Z
    *Main> exNat12
    S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))
    *Main> exInt12
    12
    *Main> exNatExp
    S (S (S (S (S (S (S (S Z)))))))
    *Main> exNatExpa
    8
    *Main>     

-}
{-
    Code to represent binary strings
-}
binary :: Integral a => a -> [Int]
binary x = reverse (bits x)
    where bits 0 = [0]
          bits 1 = [1]
          bits n = fromIntegral (rem n 2) : bits (quot n 2)
          
showDigits :: [Int] -> String
showDigits = map intToDigit

bin :: Integral a => a -> String
bin = showDigits . binary

{-
    Examples
    
        *Main> binary 8
        [1,0,0,0]
        *Main> binary 20
        [1,0,1,0,0]
        *Main> binary 100
        [1,1,0,0,1,0,0]
        *Main> showDigits it
        "1100100"
        *Main> bin 50
        "110010"
        *Main>     
-}          

{-
    Exercise 8.1
    
    Give a function 'hex' for displaying numbers in type
    class Integral in hexadecimal form, i.e., in base 16
    representation. The extra digits a, b, c, d, e, f
    for 10, 11, 12, 13, 14, 15 that you need are provided
    by intToDigit. The call hex 31 should yield "1f".
    
    Hint: it is worthwhile to provide a more general 'toBase'
          for converting a list of digits in any base {2,...,16},
          and then define hex in terms of that.

-}
-- provided solution
toBase :: Integral a => a -> a -> [Int]
toBase b n | b < 2 || b > 16 = error "base not in [2..16]"
           | n < 0           = error "negative argument"
           | otherwise       = reverse (toB b n)
    where
        toB b n | n < b     = [toInt n]
                | otherwise = toInt (rem n b) : toB b (quot n b)
                
        toInt n = fromIntegral n :: Int
        
hex :: (Integral a) => a -> String
hex = showDigits . toBase 16

oct :: (Integral a) => a -> String
oct = showDigits . toBase 8

{-
    8.2 GCD and Fundamental Theorem of Arithmetic
    ---------------------------------------------
    The fundamental theorem of arithmetic says that every n in the
    set of Natural numbers with n > 1 has a unique prime factorization.
    
    First, note that a factorization of 0 can never be unique as
    m * 0 = 0 for every number.
    
    Nether can a factorization of 1 be unique as we have n = 1^m * n
    for any n,m in the set of Natural numbers and 1^m is always 1 so
    1^m can never be unique.
    
    Euclid's Algorthim for finding the GCD:
    
        While a /= b
            do if a > b 
               then a = a - b
               else b = b - a
               
    so, for GCD(30,84)
    
                a0 = 30             b0 = 84
        a0<b0   a1 = 30             b1 = 84 - 30 = 54
        a1<b1   a2 = 30             b2 = 54 - 30 = 24
        a2>b2   a3 = 30 -24 = 6     b3 = 24
        a3<b3   a4 = 6              b4 = 24 - 6  = 18
        a4<b4   a5 = 6              b5 = 18 - 2  = 12
        a5<b5   a6 = 6              b6 = 12 - 6  =  6
        a6=b6 = 6
        
    Haskell contains a standard 'gcd' function.
    
        gcd :: Integral a => a -> a -> a
        gcd 0 0 = error "Prelude gcd: gcd 0 0 is undefined."
        gce x y = gcd' (abs x) (abs y)
                  where gcd' x 0 = x
                        gcd' x y = gcd' y (x `rem` y)
                        
    We can use this to define a function 'coprime'. Two natural
    numbers are 'coprime' or 'relatively prime' if GCD(n,m) = 1
    
-}
coprime :: Integer -> Integer -> Bool
coprime m n = (gcd m n) == 1

{-
    If two numbers, a, b, are coprime and a < b 
    then (a + b) and b are also coprime.
   
        *Main> coprime 12 25
        True
        *Main> 12 + 25
        37
        *Main> coprime 25 37
        True
        *Main> 25 + 37
        62
        *Main> coprime 37 62
        True
        *Main> 37 + 62
        99
        *Main> coprime 62 99
        True
        *Main> 62 + 99
        161
        *Main> coprime 99 161
        True
        *Main> 
-}
{-
    Thm 8.5
        If a,b in N with a > 0 or b>0, and if some divisor d of
        a and b satisfies d = xa + yb for some x,y in Z, then
        d = GCD(a,b).
        
    Euclid's GCD algo can be extended to compute the appropriate
    x,y such that GCD(a,b) = xa + yb

-}
gcde :: Integral a => a -> a -> (a,a,a)
gcde a b | a == 0 && b == 0 = error "gcd undefined"
         | a < 0  || b < 0  = error "gcd undefined"
         | a == 0           = (b, 0, 1)
         |           b == 0 = (a, 1, 0)
         | a > b            = let (k,r)   = quotRem a b
                                  (d,x,y) = gcde b r
                              in (d, y, x-k * y)
         | otherwise        = let (k,r)   = quotRem b a
                                  (d,x,y) = gcde a r
                              in (d, x-k * y, y)
                              
{-
    8.3 Integers
    ------------
    Suppose n,m,k are natural numbers. When n = m + k then we can
    view k as the difference of n and m.
    The natural way to view this is k = n - m BUT this can give
    rise to negative numbers, hence Integers.
            Z = {...,-3,-2,-1,0,1,2,3,...}
            
    (Z comes from Zahl, the German word for number)
    
    We can view integers as constructions that come from natural
    numbers.
    
-}                              
data Sgn   = P | N deriving (Eq,Show)
type MyInt = (Sgn,Natural)

myplus :: MyInt -> MyInt -> MyInt
myplus (s1,m) (s2,n) | s1 == s2           = (s1,m+n)
                     | s1 == P  && n <= m = (P,m-n)
                     | s1 == P  && n > m  = (N,n-m)
                     | otherwise          = myplus (s2,n) (s1,m) 

{-
    We can also represent integers as a 'difference pair'.
    For example, (0,3) is -3 but so is (1,4) or (2,5).
    
    8.4 Implementing Integer Arithmetic
    -----------------------------------
    If we represent natural numbers with type Natural, we can 
    represent integers as 'pairs' of naturals.
    e.g. minus 5 is represented by the pair
        (S Z, S( S( S (S (S (S Z)))))) or by (Z, S( S( S( S (S Z)))))
        
    We can create a type, NatPair, to represent an Integer and
    then implement arithmetic operations over the type.
    
-}                     
type NatPair = (Natural, Natural)

plus1 :: NatPair -> NatPair -> NatPair
plus1 (m1, m2) (n1, n2) = (m1+n1, m2+n2)

-- subtraction is just the inverse of addition so we just swap
-- the values
subtr1 :: NatPair -> NatPair -> NatPair
subtr1 (m1, m2) (n1, n2) = plus1 (m2,m1) (n2,n1)

mult1 :: NatPair -> NatPair -> NatPair
mult1 (m1,m2) (n1,n2) = (m1*n1 + m2*n2, m1*n2 + m2*n1)

eq1 :: NatPair -> NatPair -> Bool
eq1 (m1,m2) (n1,n2) = (m1 + n2) == (m2 + n1)

-- reduce a natural pair to their simplest form, which is either
-- the first or second number being equal to 0
reduce1 :: NatPair -> NatPair
reduce1 (m1,Z)       = (m1,Z)
reduce1 (Z,m2)       = (Z,m2)
reduce1 (S m1, S m2) = reduce1 (m1, m2)

{-
    Exercise 8.11
    
    Define and implement relations leq1 for <= and gt1 for >
    for difference classes of naturals.
-}
leq1 :: NatPair -> NatPair -> Bool
leq1 (m1,m2)(n1,n2) = (m1+n2) <= (m2+n1)

gt1 :: NatPair -> NatPair -> Bool
gt1 m1 m2 = not (leq1 m1 m2)

{-
    8.5-6 Rational Numbers and Rational Arithmetic
    ----------------------------------------------
    Further assumptions we'd like to make are:
        any integer can be divided by a non-zero integer to form
        a fraction or 'rational number'
        
        any rational number m/n can be 'cancelled down' to its
        lowest form by dividing m and n by the same number
        eg 12/39 cancels down to 4/13
        
    We can view rational numbers as pair constructions (m,n) with
    m,n in Z, n /= 0. In this case the pairs are 'ratio pairs'
    
    And again, we can implement operations on ratio pairs. In fact,
    Haskell has a standard implementation for rational numbers in
    the module 'Data.Ratio'
    
        data Integral a => Ratio a = a :% a deriving (Eq)
        type Rational              = Ratio Integer

    Provided functions are:
    
        %               - reduce a rational number to its 
                          simplest form
                          
        numerator       - extract numerator
        denominator     - extract denominator
        negate          - negation
        abs             - get absoute value
        signum          - return sign
        
    Examples:
    
        *Main> numerator (2 % 4)
        1
        *Main> denominator (2 % 10)
        5
        *Main> 2 % 10
        1 % 5
        *Main> abs (2 % 10)
        1 % 5
        *Main> negate (2 % 10)
        (-1) % 5
        *Main> signum (2 % 10)
        1 % 1
        *Main> signum (negate (2 % 10))
        (-1) % 1
        *Main>     
        
-}
{-
    You can represent Rational numbers as decimals using
    the following functions
-}
decForm :: Rational -> (Integer,[Int],[Int]) 
decForm x | x < 0     = error "negative argument"
          | otherwise = (q,ys,zs) 
  where 
  (q,r)   = quotRem n d 
  n       = numerator x 
  d       = denominator x 
  (ys,zs) = decF (r*10) d []

decF :: Integer -> Integer -> [(Int,Integer)] -> ([Int],[Int])
decF n d xs | r == 0        = (reverse (q: (map fst xs)),[])
            | elem (q,r) xs = (ys,zs) 
            | otherwise     =  decF (r*10) d ((q,r):xs)
     where 
     (q',r)  = quotRem n d
     q       = fromIntegral q'
     xs'     = reverse xs
     Just k  = elemIndex (q,r) xs'
     (ys,zs) = splitAt k (map fst xs')

{-
    Examples with repeating decimals
    
        *Main> decForm (133 % 999)      
        (0,[],[1,3,3])                  -- 0.13313313313313313
        *Main> decForm (1 % 7)
        (0,[],[1,4,2,8,5,7])            -- 0.14285714285714285
        *Main> decForm (2 % 7)          
        (0,[],[2,8,5,7,1,4])            -- 0.2857142857142857
        *Main> decForm (3 % 7)          
        (0,[],[4,2,8,5,7,1])            -- 0.42857142857142855
        *Main> decForm (4 % 7)
        (0,[],[5,7,1,4,2,8])            -- 0.5714285714285714
        *Main> decForm (5 % 7)
        (0,[],[7,1,4,2,8,5])            -- 0.7142857142857143
        *Main> decForm (6 % 7)  
        (0,[],[8,5,7,1,4,2])            -- 0.8571428571428571

        *Main> decForm (468 % 99)       -- 4.7272727272727275
        (4,[],[7,2])
        *Main> decForm (468 % 199)      -- 2.351758793969849
        (2,[],[3,5,1,7,5,8,7,9,3,9,6,9,8,4,9,2,4,6,2,3,1,1,5,5,7,7,8,8,9,
               4,4,7,2,3,6,1,8,0,9,0,4,5,2,2,6,1,3,0,6,5,3,2,6,6,3,3,1,6,
               5,8,2,9,1,4,5,7,2,8,6,4,3,2,1,6,0,8,0,4,0,2,0,1,0,0,5,0,2,
               5,1,2,5,6,2,8,1,4,0,7,0])   
        *Main> 

-}
{-
    Exercise 8.14
    
    Write a Haskell program to find the longest period [of repeating
    decimals] that occurs in deciaml expansions of fractions with
    numerator and denominator taken from the set {1,...,999}

-}
-- provided solution
periods :: [Rational] -> [Int]
periods xs = map periodLength xs
    where
    periodLength x = length (third (decForm x))
    third (_,_,c)  = c
    
ex814 = periods [ 1 % q | q <- [971..999] ]
-- biggest period is 1/983 which gives a period of 982
ex1div983 = decForm (1 % 983)

{-
    8.7 Irrational Numbers
    ----------------------
    
-}

