{-
    Chapter 7 - Induction and Recursion
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
{-# LANGUAGE NPlusKPatterns #-}

import Data.List
{-
    Induction and Recursion
    -----------------------
    "...mathematical induction is a method to prove things about
     objects that can be built from a finite number of ingredients
     in a finite number of steps. Such objects can be thought of
     as constructed by means of recursive definitions."
     
    [NOTE: from the examples below, one can see that in working
           out inductive proofs we are led from naive
           solutions to 'closed form' solutions.]

    General Approach for Inductive Proof
    ------------------------------------
    1. Basis: prove the 0 (empty, null, etc) case has the property P
    2. Induction step: assume the 'induction hypothesis' than 'n' has
       property P. Prove on the basis of this that n+1 has property P.

    The principal of 'mathematical induction' refers to the 
    following fact:
        For every set X in N, we have that:
          if 0 in X and (forall n in N)(n in X => n+1 in X), then X=N
          
    Example:
        Show that the sum of the angles of a convex polygon of n+3
        sides is (n+1)pi radians.
        
                      ----
                     /   | \
                    /    |  \
                    \    |  /
                     \   | /
                      ----
                      
        1. Basis: For n = 0, the sum of the angles of a convex polygon
               of 3 sides i.e. of a triangle, is pi radians. We know
               from elementary geometry that this is true
               
        2. Induction Step:
            Assume that the sum of the angles of a convex polygon
            of n+3 sides is (n+1)pi radians.
            Take a convex polygon P of n+4 sides. Then, since P
            is convex, we can decompose P into a triangle T and a
            convex polygon P' of n+3 sides (just connect edges 1
            and 3 of P). The sum of the angles of P equals the sum
            of the angles of T. i.e. pi radians, plus the sum of the
            angles of P', i.e. by the induction hypothesis, (n+1)pi
            radians. This shows that the sum of the angles of P is
            (n+2)pi radians.
            
        From 1 and 2 and the principal of mathematical induction
        the statement follows.                       
     
-}
{-
    Sum of first n Odd Numbers
    --------------------------
    Hypothesis: The sum of the first n odd numbers equals n^2.
        
    Basis: 
        For n=1, k=1 sigma (2k-1)=1=1^2
        
    Induction Step:
        Assume n, k=1, sigma(2k - 1) = n^2
        We have to show that n+1, k=1, sigma(2k - 1) = (n+1)^2
        
        sigma^(n+1)_(k=1)(2k-1) = n^2 +2n + 1 = (n+1)^2
-}
-- code for summing odd numbers, sumOdds is a closed formula that
-- will perform better on large input than sumOdds'
sumOdds' :: Integer -> Integer
sumOdds' n = sum [2*k - 1 | k <- [1..n] ]

sumOdds :: Integer -> Integer
sumOdds n = n^2

ex73 = sumOdds' 1000 == sumOdds 1000

{-
    Sum of Even Numbers
    -------------------
    Hypothesis: sum of the first n even numbers is n(n+1)/2
    
    Basis: Putting k = 1 givens 2 = 1 * 2 which is correct
    
    Induction Step:
        Assume n, k=1, sigma 2k = n(n+1)
        Then n+1, k=1, sigma 2k = n, k=1, sigma 2k + 2(n+1)
        Using the induction hypothesis, this is equal to
          n(n+1) + 2(n+1) = n^2 + 3n + 2 = (n+1)(n+2)
        
        From which follows
            n, k=1 sigma k = n(n+1) / 2
          
-}
sumEvens' :: Integer -> Integer
sumEvens' n = sum [ 2*k | k <- [1..n] ]

sumEvens :: Integer -> Integer
sumEvens n = n * (n+1)

ex74 = sumEvens' 1000 == sumEvens 1000

sumInts :: Integer -> Integer
sumInts n = (n * (n+1)) `div` 2

{-
    Summing Squares
    ---------------
    Looking for a pattern
    
    1^2 + 2^2                             = 5  = (2*3*5)/6
    1^2 + 2^2 + 3^2             = 5 + 9   = 14 = (3*4*7)/6
    1^2 + 2^2 + 3^2 + 4^2       = 14 + 16 = 30 = (4*5*9)/6
    1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 30 + 25 = 55 = (5*6*11)/6
    
    Suggesting the general rule:
        1^2 + ... + n^2 = n(n+1)(2n+1) / 6
    
    NOTE: looking for the inductive proof leads to the 'closed
          form' solutions
-}
-- naive and closed forms for summing squares
sumSquares' :: Integer -> Integer
sumSquares' n = sum [ k^2 | k <- [1..n] ]

sumSquares :: Integer -> Integer
sumSquares n = (n * (n+1) * (2*n+1)) `div` 6

ex75 = sumSquares' 1000 == sumSquares 1000

{-
    Summing Cubes
    -------------
    Looking for the pattern:
    
    1^3 + 2^3                                     = 9 = (1+2)^2
    1^3 + 2^3 + 3^3             = 9 + 27    = 36  = (1+2+3)^2
    1^3 + 2^3 + 3^3 + 4^3       = 36 + 64   = 100 = (1+2+3+4)^2
    1^3 + 2^3 + 3^3 + 4^3 + 5^3 = 100 + 125 = 225 = (1+2+3+4+5)^2
    
    Suggesting the general (closed form) solution:
    
    1^3 + ... + n^3 = (1 + ... + n)^2
    
    And (1 + ... + n) reduces to (n * (n+1))/2
    So the general rule reduces to:
            ((n * (n+1))/2 )^2
-}
-- naive and closed forms for summing cubes
sumCubes' :: Integer -> Integer
sumCubes' n = sum [ k^3 | k <- [1..n] ]

sumCubes :: Integer -> Integer
sumCubes n = (n * (n+1) `div` 2)^2

ex77 = sumCubes' 1000 == sumCubes 1000

{-
    7.2 Recursion over the Natural Numbers
    --------------------------------------
    Induction over natural numbers works because we can envision
    any natural n as starting from 0 and applying +1 a finite
    number of times.
    
    We can use this fact about natural numbers to create our
    own Haskell implementation.
    
    We can define the successor operation, +1, recursively
    
                m + 0 := m                  -- base case
            m + (n+1) := (m + n) + 1        -- recursive case
            
    Once we have addition we can write a recursive definition
    for multiplication
    
                m * 0 := 0                  -- base case
            m * (n+1) := (m*n)+ m           -- recursive case

-}
data Natural = Z            -- zero
             | S Natural    -- succedent number, (n + 1)
    deriving (Eq, Show)
    
n0 = Z                      -- natural number 0
n1 = (S Z)                  -- natural number 1
n2 = S(S Z)                 -- natural number 2
n3 = S(S (S Z))             -- natural number 3
n4 = S(S (S (S Z)))         -- natural number 4
    
plus :: Natural -> Natural -> Natural    
plus m Z     = m
plus m (S n) = S (plus m n)

ex0p1 = (plus n0 n1) == n1
ex1p3 = (plus n1 n3) == n4

n5 = plus n2 n3
n6 = plus n2 n4

mult :: Natural -> Natural -> Natural
mult m Z = Z
mult m (S n) = (m `mult` n) `plus` m

ex2m3 = (mult n2 n3) == n6

{-
    Show the following laws for addition
    
    identity        m + 0 = m
    commutativity   m + n = n + m
    associativity   m + (n + k) = (m + n) + k
-}

addId    = plus n4 n0           == n4
addComm  = plus n2 n3           == plus n3 n2
addAssoc = plus n1 (plus n2 n3) == plus (plus n1 n2) n3

{-
    Show the following laws for multiplication
    
    identity        m * 1 = m
    distribution    m * (n+k) = m*n + m*k
    associativity   m * (n*k) = (m*n) * k
    commutativity   m * n = n * m

-}
multId    = mult n5 n1           == n5
multDist  = mult n2 (mult n2 n1) == mult (mult n2 n2) n1
multAssoc = mult n1 (mult n2 n3) == mult (mult n1 n2) n3
multComm  = mult n2 n3           == mult n3 n2

{-
    Define a recursion for exponentiation
    
                    m^0 := 1                    -- base case
                m^(n+1) := (m^n) * m            -- recursive case
                
-}
expn :: Natural -> Natural -> Natural
expn m Z = (S Z)
expn m (S n) = mult (expn m n) m

ex712 = expn n2 n3          -- S (S (S (S (S (S (S (S Z)))))))

{-
    Recursive definition for <=
    
               0 <= m,
        m + 1 <= n + 1 if m <= n
        
    Which, once defined, allows us to build > and == using 
    negation

-}
leq, geq, gt, lt :: Natural -> Natural -> Bool
leq Z _         = True
leq (S _) Z     = False
leq (S m) (S n) = leq m n

geq m n = leq n m
gt  m n = not (leq m n)
lt  m n = gt n m


ex3leq5  = leq n3 n5
ex5geq3  = geq n5 n3
ex5gt2   = gt  n5 n2
ex2lt5   = lt  n2 n5

{-
    Exercise 7.14
    Implement an operation for cut-off subtraction 
        subtr (S (S (S Z))) (S (S (S (S Z))) should yield Z
        
    Ans:  Cut-off subtraction is defined as
    
        0     if m < n          -- base case
        n - m if n >= m         -- recursive case
        
-}
subtr :: Natural -> Natural -> Natural
subtr Z _         = Z           -- m == 0
subtr m Z         = m           -- n == 0
subtr (S m) (S n) = subtr m n   -- every other case

ex714 = subtr n3 n4             -- Z

{-
    Exercise 7.15
    
    Implement operations quotient and remainder on naturals.
    Dividing a by b yields quotient q and remainder r with
    0 <= r < b, according to the formula a = q * b + r.
    Hint: you will need the subtr function from exercise 7.14.
    
-}
-- provided solution
qrm :: Natural -> Natural -> (Natural,Natural)
qrm m n | gt n m    = (Z,m)
        | otherwise = (S (fst qr), snd qr) 
    where qr = qrm (subtr m n) n

quotient :: Natural -> Natural -> Natural
quotient m n = fst (qrm m n)

remainder :: Natural -> Natural -> Natural
remainder m n = snd (qrm m n)

{-
    7.3 The Nature of Recursive Definitions
    ---------------------------------------
    The following definition does not serve as an operation over
    natural numbers
    
                      f(0) := 1
                    f(n+1) := f(n + 2)
                    
    as it does not define unique values for f(1),f(2),f(3), etc
    
    The following DOES guarantee a proper defintion
    
                    f(0) := c
                  f(n+1) := h(f(n))
                  
    The pattern is known as 'structural' or 'primitive recursion'; 
    it allows 'c' to depend on a number of parameters upon which 'f'
    will also depend.
    
    Definition by structural recursion works like this: take a
    natural number n and view it as
        
                1 + ... + 1 + 0   
        
    and replace the 0 with c and each 1+ with h
     
                h( ... (h(c)) ..)
                
    this can be generalized as a 'foldn'

-}
foldn :: (a -> a) -> a -> Natural -> a
foldn h c Z     = c
foldn h c (S n) = h (foldn h c n)

{- an example using foldn, produces n exclamation marks

        exclaim S(S (S (S Z)))
     -> foldn ('!' :) [] (S (S (S (S Z))))
     -> '!' : (foldn ('!' :) [] (S (S (S Z))))
     -> '!' : ('!' : (foldn ('!' :) [] (S (S Z))))
     -> '!' : ('!' : ('!' : (foldn ('!' :) [] (S Z))))
     -> '!' : ('!' : ('!' : ('!' : (foldn ('!' :) [] Z))))
     -> '!' : ('!' : ('!' : ('!' : [])))
     -> '!' : ('!' : ('!' : ['!']))
     -> '!' : ('!' : ['!','!'])
     -> '!' : ['!','!','!']
     -> ['!','!','!','!']
     -> "!!!!"
    
-}
exclaim :: Natural -> String
exclaim = foldn ('!' :) []

{-
    We can redefine our 'plus' using 'foldn', here 'm' becomes
    our 'c' and the successor function becomes our 'h'
    Note: S is a constructor; constructors are also functions
          so S is equivalent to the lambda function 
                (\ n -> S n)
                
    Example:
            plus' (S Z) (S (S (S Z)))       -- n1 + n3
         -> foldn S (S Z) (S (S (S Z)))
         -> S (foldn S (S Z) (S (S Z)))
         -> S (S (foldn S (S Z) (S Z)))
         -> S (S (S (foldn S (S Z) Z)))
         -> S (S (S (S Z)))                 -- n4
         
    mult can also be defined in terms of foldn and expn
    redfiend in terms of or new mult
         
-}
plus' :: Natural -> Natural -> Natural
plus' = foldn S

mult' :: Natural -> Natural -> Natural
mult' m = foldn (plus m) Z

expn' :: Natural -> Natural -> Natural
expn' m = foldn (mult m) (S Z)

exPlus = plus n1 n3 == plus' n1 n3
exMult = mult n2 n3 == mult' n2 n3
exExpn = expn n2 n3 == expn' n2 n3

{-
    Exercise 7.14
    Implement subtr as a foldn. You need a function for
    the predecessor function, p, such that:
    
            x - 0     = x
            x - (n+1) = p(x - n)
-}
subtr' :: Natural -> Natural -> Natural
subtr' = foldn pre
    where 
        pre :: Natural -> Natural
        pre Z     =  Z
        pre (S n) = n
        
ex716 = subtr n3 n4 == subtr' n3 n4         

{-
    Exercise 7.18
    
    A bitlist is a list of zeros and ones. Consider the 'bittest' 
    code below for selecting the bitlists WITHOUT consecutive
    zeros.
    
    1. How many bitlists of length 0, 1 2, and 3 satisfy bittest?
    2. ...

-}
bittest :: [Int] -> Bool
bittest []       = True
bittest [0]      = True
bittest (1:xs)   = bittest xs
bittest (0:1:xs) = bittest xs
bittest _        = False

ex7180 = bittest []                          -- length 0
ex7181 = bittest [0] && bittest [1]          -- length 1
ex7182 = bittest [0,1] && bittest [1,0]      -- length 2
ex7183 = bittest [1,1,1]                     -- length 3
      && bittest [1,1,0]
      && bittest [1,0,1] 
      && bittest [0,1,1]
      && bittest [0,1,0]

{-
    Exercise 7.20
    
    The Catalan numbers are given by the following recursion
        C_0 = 1
        C_(n+1) = C_0C_n + C_1C_(n-1) +...+C_(n-1)C_1 + C_nC_0
        
    Which gives
        [1,1,2,5,14,42,132,429,1430,4862,16796,58786,...]
        
    Use recursion to give a Haskell implementation. Why is this
    not an efficient way to compute the Catalan numbers?
-}      
-- provided solution (needs the 'pragma'
-- execution time grows as 'n' grows; fast only for numbers < 10
catalan :: Integer -> Integer
catalan 0 = 1
catalan (n+1) = sum [ (catalan i) * (catalan (n-i)) | i <- [0..n] ]

      
-- from STAL.hs
display :: Int -> String -> IO ()
display n str = putStrLn (display' n 0 str)
  where 
  display' _ _ [] = []
  display' n m (x:xs) | n == m    = '\n': display'  n   0  (x:xs)
                      | otherwise =  x  : display'  n (m+1)  xs 

