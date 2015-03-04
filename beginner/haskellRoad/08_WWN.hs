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
    Irrational numbers, like the square root of 2, cannot be represented
    by rational fractions; instead, scientific notation is used. For
    example, the square root of 2 is represented as:
            1.41421e+06
    with the decimal portion, 1.41421, called the 'mantissa' and
    the remaining portion, 06, the 'exponent'.  Such numbers are
    represented, in computing, as Float's (single precision) or
    Double's (double precision).
    
    Floating point numbers are stored as pairs (m,n) where m is the
    mantissa (significand) and n is the exponent.
    
    Haskell has a number of functions for working with floating
    point values.
-}
ex871  = floatRadix (sqrt 2)     -- returns the base of a number
ex872  = decodeFloat (sqrt 2)    -- returns mantissa and exponent
ex872a = fromIntegral (fst ex872) 
       * (fromIntegral ex871)^^(snd ex872)   -- reconstruct number
ex872b = encodeFloat (fst ex872) (snd ex872) -- reconstruct number
ex873  = floatDigits (sqrt 2)  -- # of digits in the mantissa
 
ex874  = scaleFloat 4 (sqrt 2)
ex874a = ex874 == 2^4 * sqrt 2 
 
ex875   = significand (sqrt 2)  -- mantissa scaled to between (-1,1)
ex875a  = exponent (sqrt 2)     -- scaled exponent
ex875b  = ex875 * 2^ex875a == scaleFloat ex875a ex875

{-

    *Main> ex871
    2
    *Main> ex872
    (6369051672525773,-52)
    *Main> ex872a
    1.4142135623730951
    *Main> ex872b
    1.4142135623730951
    *Main> ex873
    53
    *Main> ex874
    22.627416997969522
    *Main> ex874a
    True
    *Main> ex875
    0.7071067811865476
    *Main> ex875a
    1
    *Main> ex875b
    True
    *Main>     

-}
{-
    8.8 The Mechanic's Rule
    -----------------------
    Sequences of fractions can be used find approximations to
    real numbers that are, themselves, NOT fractions. A well
    known algorithm for generating such sequences is 'The
    Mechanic's Rule' (also called Newton's method).
    
    The two functions defined below use the Predlude functions:
        recip   takes the reciprocal of a fraction
        iterate applies the function to the result of the previous
                application
-}
mechanicsRule :: Rational -> Rational -> Rational
mechanicsRule p x = (1 % 2) * (x + (p * (recip x)))

mechanics :: Rational -> Rational -> [Rational]
mechanics p x = iterate (mechanicsRule p) x

sqrtM :: Rational -> [Rational]
sqrtM p | p < 0    = error "negative argument"
        | otherwise = mechanics p s
    where
        s  = if xs == [] then 1 else last xs
        xs = takeWhile (\ m -> m^2 <= p) [1..]
        
-- following examples demonstrate that the algorithm 
-- converges very fast        
ex881  = take 5 (sqrtM 2)
ex881a = fromRational (last ex881)
ex881b = sqrt 2
      
ex882  = take 7 (sqrtM 4)
ex882a = fromRational (last ex882)
ex882b = sqrt 4

ex883  = take 5 (sqrtM 50)
ex883a = fromRational (ex883 !! 3)      -- 4th step
ex883b = fromRational (last ex883)      -- 5th step
ex883c = sqrt 50                        

{-

        *Main> ex881
        [1 % 1,3 % 2,17 % 12,577 % 408,665857 % 470832]
        *Main> ex881a
        1.4142135623746899
        *Main> ex881b
        1.4142135623730951
        *Main> 
        *Main> ex882
        [2 % 1,2 % 1,2 % 1,2 % 1,2 % 1,2 % 1,2 % 1]
        *Main> ex882a
        2.0
        *Main> ex882b
        2.0
        *Main> 
        *Main> ex883
        [7 % 1,99 % 14,19601 % 2772,768398401 % 108667944,1180872205318713601 % 167000548819115088]
        *Main> ex883a
        7.0710678118654755
        *Main> ex883b
        7.0710678118654755
        *Main> ex883c
        7.0710678118654755
        *Main> 

-}      
       
{-  8.9 Reasoning about Reals    
    -------------------------
    [Note: text briefly discusses continuous functions and limits
           and applies the ideas to sequences of Real numbers]
    
    We can set our tolerance level, or how accurate we need the
    approximation of a real value to be, using an 'epsilon' value
    The following functions use this approach to computing square
    roots.
-}

approximate :: Rational -> [Rational] -> Rational
approximate eps (x:y:zs) | abs (y-x) < eps = y
                         | otherwise       = approximate eps (y:zs)

apprx :: [Rational] -> Rational
apprx = approximate (1/10^6)

mySqrt :: Rational -> Rational
mySqrt p = apprx (sqrtM p)

ex891 = (fromRational (mySqrt 2), sqrt 2)
ex892 = (fromRational (mySqrt 7), sqrt 7)
ex893 = (fromRational (mySqrt 50), sqrt 50)

{-

    *Main> ex891
    (1.4142135623730951,1.4142135623730951)
    *Main> ex892
    (2.6457513110646933,2.6457513110645907)
    *Main> ex893
    (7.0710678118654755,7.0710678118654755)
    *Main> 

-}
                        
{- 
    8.10 Complex Numbers
    --------------------
    In the realm of real numbers we cannot solve the equation
            
            x^2 + 1 = 0 -> x^2 = -1 -> x = sqrt -1
    
    because the square root of -1 does not exist (is undefined)
    in the realm of real numbers.
    
    As a result, the number domain was extended to that of
    'complex numbers' which introduces an entity, i, the
    imaginary unit, with the value i^2 = -1.  Complex numbers,
    thus, have 'real' and 'imaginary' components:
    
            x + iy 
            
        where x and y are arbitrary real numbers
              'x'  is the 'real' part and
              'iy' is the imaginary part
              
    And
    
        (x + iy) + (u + iw) = (x + u) + i(y + w)
        (x + iy) - (u + iw) = (x+iy) + (-u + -iw) = (x - u)+ i(y-w)
        
        (x + iy)(u + iw) = xu + iyu + ixw + i^2yw   
                         = xu + iyu + ixw + (-1)yw
                         = xu + i(yu + xw) - yw
                         = (xu - yw) + i(yu + xw)
                         
        x + iy    x + iy   u - iw   xu  + yw      yu  - xw
        ------ =  ------ x ------ = --------- + i ---------
        u + iw    u + iw   u - iw   u^2 + w^2     u^2 + w^2
                
    
    There is a standard module, Complex.hs, in Haskell that
    implements complex numbers.  
    
    The complex number z = x + iy can be represented geometrically 
    in two ways
    
    1. Associate 'z' with the point coordiantes (x,y) in the
       plane R^2. R^2 is then the 'complex plane'
       
    2. Associate 'z' with the 2-dimensional vector with components
       x and y. Think of this as a free vector i.e. a vector that
       can be moved around freely as long as it does not change
       direction.
    
    The two representations can be combined by attaching the
    vector z to the origin of the plane.
    
  
                              iy
                                |    z = (x + iy)
                                |  /
                                | /
                                |/
                   -1  ---------o----------- 1 --> x
                                |\  
                                | \
                                |  \
                                |   z' = (x - iy)
                               -i
    The horizontal axis, through the origin, (x-axis) is the 'real
    axis'. The vertical axis through the origin (y-axis) is the
    'imaginary axis'.
    
    The 'conjugate' of the vector z is z' (normally shown as a z
    topped with a overscore); it is the reflection of z.
    
    The 'magnitude' (modulus or absolute value) of the complex
    number 'z' is the length r of the z vector. Notated as |z|
    The magnitude of z = x + iy is the square root of x^2+y^2.
   
    The 'phase' or 'argument' of a complex number 'z' is the angle
    theta of the vector z. The phase theta of a vector of magnitude 1
    is given by the vector cos(theta) + i sin(theta), so the phase
    of z = x + iy is given by atan(y/x) for x > 0 and atan(y/x)+pi
    for x < 0. If x = 0 then phase is 0.  The Haskell phase function
    uses atan2 to return the value in the correct quadrant.
    
    The polar representation of a complex number z = x + iy is
    r angle theta where r = |z| and theta = arg(z) [phase z]. To
    get back to the vector sum, use mkPolar.
    
    Use 'cis' to convert a phase theta to a vector in the unit
    circle.
 
    In the complex plane, positive real numbers have phase 0 or
    2k*pi while negative real numbers have phase pi or (2k + 1)pi.
    Multiplying two negative real numbers means multiplying
    their values and adding their phases, so we get phase 2pi which
    is the same as phase 0, modulo 2k*pi
 
    A complex number, in Haskell, is written as:
    
                (x :+ y)
-}
z :: Complex Double
z      = 9 :+ 3

theta :: Double
theta = 30.0

ex8101 = realPart  z
ex8102 = imagPart  z
ex8103 = conjugate z
ex8104 = magnitude z
ex8105 = phase     z
ex8106 = polar     z
ex8107 = mkPolar (fst ex8106) (snd ex8106)
ex8108 = cis theta

{-
    *Main> z
    9.0 :+ 3.0                                  -- complex number
    *Main> ex8101
    9.0                                         -- real part
    *Main> ex8102
    3.0                                         -- imaginary part
    *Main> ex8103
    9.0 :+ (-3.0)                               -- conjugate
    *Main> ex8104
    9.486832980505138                           -- magnitude
    *Main> ex8105
    0.3217505543966422                          -- phase
    *Main> ex8106
    (9.486832980505138,0.3217505543966422)      -- polar
    *Main> ex8107
    9.0 :+ 3.0                                  -- mkPolar
    *Main> ex8108
    0.15425144988758405 :+ (-0.9880316240928618)    -- cis 30.0
    *Main> 

-}

-- solve a quadratic equation
solveQ :: (Complex Float, Complex Float, Complex Float)
       -> (Complex Float, Complex Float)
solveQ = \ (a,b,c) -> if a == 0 
                      then error "not quadratic"
                      else let d = b^2 - 4 * a * c
                           in ((- b + sqrt d) / 2 * a,
                               (- b - sqrt d) / 2 * a)
                               
ex8109  = solveQ (1, 0, -1)
ex8109a = solveQ (1, 0,  1)                               
ex8109b = solveQ (1, 0, -2)
ex8109c = solveQ (1, 0,  2)
{-
    *Main> ex8109
    (1.0 :+ 0.0,(-1.0) :+ 0.0)
    *Main> ex8109a
    (0.0 :+ 1.0,0.0 :+ (-1.0))
    *Main> ex8109b
    (1.4142135 :+ 0.0,(-1.4142135) :+ 0.0)
    *Main> ex8109c
    (0.0 :+ 1.4142135,0.0 :+ (-1.4142135))
    *Main> 
-}
