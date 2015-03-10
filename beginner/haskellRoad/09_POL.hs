{-
    Chapter  - Polynomials
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
module POL where

import Data.Ratio
import Polynomials

{-
    "Polynomicals ... are functions that can be represented by
    a finite number of additions, subtractions, and multiplications
    with one independent variable."
    
    The closed forms we found in the chapter on induction are all
    polynomial forms.
    
    9.1 Difference Analysis of Polynomial Sequences
    -----------------------------------------------
    
-}
-- example of a 2nd degree polynomial function
ex911a = take 15 (map (\ n -> 2 * n^2 + n + 1) [0..])

-- a difference function, d(f)
difference :: (Num a, Num b) => (a -> b) -> a -> b
difference f x = f (x+1) - f x

-- specific transform function over natural numbers
difs :: [Integer] -> [Integer]
difs [] = []
difs [n] = []
difs (n:m:ks) = m-n : difs (m:ks)

ex911b = difs ex911a

{- 
    The difference function, d(f), of a polynomial function, f, 
    is, itself, a polynomial function.
   
    For example, the difference function for the polynomial function
   
        f = \n.(2n^2 + n + 1)       -- 'difference' sequence above
        
    is
        d(f) = \n.(2(n+1)^2 + (n+1) + 1 - (2n^2 + n + 1)
             = \n.4n + 3
             
    So, we have a polynomial function producing the sequence
    
        [1,4,11,22,37,56,79,106,137,172,211,254,301,352,407]

    A 'difference' function, which, applied to the above functions,
    gives:
    
        [3,7,11,15,19,23,27,31,35,39,43,47,51,55]

    A similar 'difs' function that transforms an sequence of natural
    numbers, which, when applied to the above polynomial function
    gives the same result as 'difference' and, finally, the 
    difference function represented as a polynomial function itself,
    giving the same result as 'difference' and 'difs' when applied
    to the example polynomial function.
    
    In otherwords, we get Proposition 9.2
        If f is a polynomial function of degree k then d(f) is
        a polynomial function of degree k - 1.
    
-}   
ex911c = take 15 (map (\ n -> 4 * n + 3) [0..])
     == take 15 (difs (map (\ n -> 2 * n^2 + n + 1) [0..]))

ex911d = take 15 (map (\ n -> 4 * n + 3) [0..])   
     == take 15 (map (difference (\ n -> 2 * n^2 + n + 1)) [0..])
     
{-
    It follows, from Prop 9.2, that if f is a polynomial function
    of degree k then d^k(f) will be a constant function (a
    polynomial function of degree 0).
    
    Example of computing difference sequences until we hit a
    constant sequence:
    
    -12  -11   6   45    112     213     354      541
        1   17   39   67     101     141     187
          16   22   28    34      40      46
             6    6     6      6      6
             
    The sequence of third differences is constant, therefore,
    the form of the original sequence was a polynomial of degree
    3.
    
    To find the next number in the sequence, add the numbers
    at the end of each row:  6 + 46 + 187 + 541 = 780.
    
    Charles Babbage used these observations to design his
    'difference engine'. Below is a Haskell 'difference engine'
    function.

-}     
difLists :: [[Integer]] -> [[Integer]]
difLists [] = []
difLists lists@(xs:xss) =
    if constant xs then lists else difLists ((difs xs):lists)
    where
        constant (n:m:ms) = all (==n) (m:ms)
        constant _        = error "lack of data or not a poly fn"
        
ex912a = difLists [[-12,-11,6,45,112,213,354,541,780,1077]]

-- compute the list of first differences (first value in each
-- computed row of differences)
firstDifs :: [Integer] -> [Integer]
firstDifs xs = reverse $ map head (difLists [xs])

ex912b = firstDifs [-12,-11,6,45,112,213,354,541,780,1077]

-- compute first differences on the function itself
firstDfs :: (Num a, Num b) => (a -> b) -> [b]
firstDfs f = f 0 : firstDfs (difference f)

ex912c = take 10 (firstDfs (\ n -> 2 * n^2 + n + 1))

-- to find the next number in the sequence, we add the last
-- element of each row of differences
-- compute the list of last differences
genDifs :: [Integer] -> [Integer]
genDifs xs = map last (difLists [xs])

ex912d = genDifs [-12,-11,6,45,112,213,354,541,780,1077]

-- a new list of last elements of difference lists is computed
-- from the current one by keeping the constant element (d_1)
-- and replacing each d_i+1 by d_i + d_i+1
nextD :: [Integer] -> [Integer]
nextD [] = error "no data"
nextD [n] = [n]
nextD (n:m:ks) = n : nextD (n+m : ks)

-- the next element of the original sequence is given by the
-- last element of the new list of last elements of difference lists
next :: [Integer] -> Integer
next = last . nextD . genDifs

ex912e = next [-12,-11,6,45,112,213,354,541,780,1077]    -- 1438

-- continue a list of polynomial form from given elements
continue :: [Integer] -> [Integer]
continue xs = map last (iterate nextD differences)
    where
        differences = nextD (genDifs xs)
        
ex912f = take 20 (continue [-12,-11,6,45,112,213,354,541,780,1077])
  
-- compute the degree of a polynomial sequence
degree :: [Integer] -> Int
degree xs = length (difLists [xs]) - 1

ex912g = degree  [-12,-11,6,45,112,213,354,541,780,1077]     -- 3

-- continue a list of sums of squares
ex912h = take 10 (continue [1,5,14,30,55])

-- continue a list of sums of cubes
ex912i = take 10 (continue [1,9,36,100,225,441])

{-
    Exercise 9.3
    
    What continuation do you get for [3,7,17,39,79,143]?
    Can you reconstruct the polynomial function that was used
    to generate the sequence?
    
    Continuation: [237,367,539,759,1033,1367,1767,2239,2789,3423]
    degree:       3
    diff lists :  [[6,6,6],[6,12,18,24],[4,10,22,40,64],
                   [3,7,17,39,79,143]]

    Provided Solution: \n.n^3 + 3n + 3
        (no explanation on how to go about finding the solution)
-}
ex913a = take 10 (continue [3,7,17,39,79,143])
ex913b = degree [3,7,17,39,79,143]
ex913c = difLists [[3,7,17,39,79,143]]
ex913d = take 10 (map (\ n -> n^3 + 3 * n + 3) [0..])

{-
    9.2 Gaussian Elimination
    ------------------------
    Difference analysis allows us to continue a sequence given
    in polynomial form; Gaussian Elimination gives us an algorithm
    for finding the polynomial form.
    
    If we know a sequence a0,a1,a2,a3,... has a polynomial form of 
    degree 3 then we know that the form is:
    
            a + bx + cx^2 + dx^3
            
    which means we can find the polynomial form by solving the 
    following quadruple of independent linear equations (none of
    the equations can be written in terms of the others) in
    a, b, c, d:
    
                                a = a0
                    a + b + c + d = a1
                 a + 2b + 4c + 8d = a2
                a + 3b + 9c + 27d = a3
                
    Example:
        Find and solve the appropriate set of equations for
        the sequence:
            [ -7, -2, 15, 50, 109, 198, 323 ]
            
        First, find the degree of the sequence, which is 3
        which leads to the following set of equations:
        
                                a = -7
                    a + b + c + d = -2
                 a + 2b + 4c + 8d = 15
                a + 3b + 9c + 27d = 50

        Eliminating a and rewriting gives:

                        b + c + d = 5
                     2b + 4c + 8d = 22
                    3b + 9c + 27d = 57
                    
        Next, eliminate d
        
          r1*8   8b + 8c + 8d = 40
               - 2b + 4c + 8d = 22
                 -----------------
                 6b + 4c + 0  = 18  -> 3b + 2c = 9
                 
        r1*27 27b + 27c + 27d = 135
            -  3b +  9c + 27d =  57
              ---------------------
              24b + 18c +  0  =  78
              
        Giving the two equations:
        
                        3b +  2c =  9
                       24b + 18c = 78
                       
        From which we can now eliminate c:
        
                r1*9   27b + 18c = 81
                     - 24b + 18c = 78
                       --------------
                        3b +  0  = 3
                        
        So, we know b = 1 and we can get c from
            3b + 2c = 9 -> 3(1) + 2c = 9
                        -> 2c = 6
                        ->  c = 3
                        
        And we get d from:
            b + c + d = 5 -> 1 + 3 + d = 5
                          -> d = 1
                          
        Thus, a = -7, b = 1, c = 3 and d = 1 and our polynomial
        form is:
                  d*n^3 + c*n^2 + b*n +   a
            ->    1*n^3 + 3*n^2 + 1*n + (-7)
            -> \n.  n^3 + 3*n^2 +   n -   7
                                         
-}
ex924a = degree [ -7, -2, 15, 50, 109, 198, 323 ]
ex924b = take 7 (map (\n -> n^3 + 3*n^2 + n -7) [0..])
      == [ -7, -2, 15, 50, 109, 198, 323 ]                  -- True
      
{-
    Exercise 9.5
    
    Find the appropriate set of equations for the sequence
    
        [13, 21, 35, 55, 81, 113, 151]
        
    and solve it.
    
    Solution:
    
        degree of the sequence is 2 so the form is:
        
                c*n^2 + b * n + a 
        
        which leads to the following set of equations:
        
                                a = 13
                        a + b + c = 21
                      a + 2b + 4c = 35
                      
        Substituting for a gives:
                           b +  c =  8
                          2b + 4c = 22
                          
        Eliminate c
        
            r1 * 4      4b + 4c = 32
                      - 2b + 4c = 22
                        ------------
                        2b +  0 = 10  -> b = 5
                        
        So c is given by:
                    5 + c = 8 -> c = 3
                    
        And the solution is
                c*n^2 + b * n +  a
                3*n^2 + 5 * n + 13
                
             \n.3n^2 + 5n + 13            
-}      
ex925a = degree [13, 21, 35, 55, 81, 113, 151]
ex925b = take 7 (map (\n -> 3*n^2 + 5*n + 13) [0..])
      == [13, 21, 35, 55, 81, 113, 151]

{-
    We can look at solving linear equations as matrix manipulation
    For example, the quadruple linear equations in a, b, c, d
    for a 3rd degree polynomial can be represented as:
    
            1 0 0  0 a0
            1 1 1  1 a1
            1 2 4  8 a2
            1 3 9 27 a3
            
    And to solve this, we transform it into an equivalent matrix
    of 'echelon form' or 'left triangular form', i.e. a matrix
    in the following form:
    
            a0  a01 a02 a03 b0
             0  a11 a12 a13 b1
             0   0  a22 a23 b2
             0   0   0  a33 b3
             
    From this we can compute the value of 'd' from the last row
    Eliminate 'd' from the 3rd row to find 'c'
    Use the values of 'd' and 'c' to find 'b' from the 2nd row
    And finally, use the values of b,c,d to find a from the 1st row.
    
    Two type definitions and a number of functions to help us handle 
    matrices are given below
    
-}
type Matrix = [Row]
type Row    = [Integer]

rows, cols :: Matrix -> Int
rows m = length m
cols m | m == []   = 0
       | otherwise = length (head m)
       
-- build a matrix from a list generated by a polynomial
genMatrix :: [Integer] -> Matrix
genMatrix xs = zipWith (++) (genM d) [ [x] | x <- xs ]
    where
        d      = degree xs
        genM n = [ [ (toInteger x^m) | m <- [0..n] ] | x <- [0..n]]

ex92a = genMatrix [ -7, -2, 15, 50, 109, 198, 323 ]

{-
    Forward Gaussian Elimination
    ----------------------------
    
    The process of transforming a matrix to 'row echelon' form is
    called 'forward elimination'.  To transform a matrix to echelon
    form:
    
    1. if the number of rows or columns in the matrix is zero then
       the matrix is already in echelon form
       
    2. if every row of rs begins with a 0 then the echelon form of
       rs can be found by putting 0's in front of the echelon form
       of 'map tail rs'
       
    3. if rs has rows that do not start with 0, then take the first
       one of these, pivot, and use it to eliminate the leading 
       coefficients from the other rows. This gives a matrix of the
       form
       
            a00 a01 a02 a03 ... b0
             0  a11 a12 a13 ... b1
             0  a21 a22 a23 ... b2
             0  a31 a32 a33 ... b3   
             .   .   .   .   .   .
             .   .   .   .   .   .
             .   .   .   .   .   .
             0  an1 an2 an3 ... bn

        where the first row is the pivot row. Then all that remains
        is to put the following submatrix in echelon form:
                        
            a01 a02 a03 ... b0
            a11 a12 a13 ... b1
            a21 a22 a23 ... b2
            a31 a32 a33 ... b3
             .   .   .   .   .
             .   .   .   .   .
             .   .   .   .   .
            an1 an2 an3 ... bn
            
-}
-- adjust one row by another
adjustWith :: Row -> Row -> Row
adjustWith (m:ms) (n:ns) = zipWith (-) (map (n*) ms) (map (m*) ns)

-- reduce a matrix to echelon form (code from HUGS Matrix.hs)
echelon :: Matrix -> Matrix
echelon rs
    | null rs || null (head rs) = rs
    | null rs2                  = map (0:) (echelon (map tail rs))
    | otherwise                 = piv : map (0:) (echelon rs')
    where
        rs'             = map (adjustWith piv) (rs1 ++ rs3)
        (rs1, rs2)      = span leadZero rs
        leadZero (n:_ ) = n == 0
        (piv : rs3)     = rs2
                
ex92b = echelon [[1,0,0,0,-7],[1,1,1,1,-2],[1,2,4,8,15],[1,3,9,27,50]]

{-
    Backward Gaussian Elimination
    -----------------------------

    We compute the values of variables from a matrix in echelon
    form using 'backward Gaussian elimination'.
    
    First, we compute the value of the variable in the last row
    then we eliminate that variable from the other rows to get
    smaller echelon form matrix; then repeat until the values of
    all variables are found.    
    
    'eliminate' reduces the echelon form matrix to a smaller
    echelon matrix by eliminating the last row in the original
    
    'backsubst' reduces the variables using Rational numbers
    i.e. if we know ax = c then x = c/a
    
    We can then combine all this in one function: solvSeq
    
-}        
eliminate :: Rational -> Matrix -> Matrix
eliminate p rs = map (simplify c a) rs
    where
        c = numerator   p
        a = denominator p
        simplify c a row = init (init row') ++ [a*d - b*c]
            where
                d    = last row
                b    = last (init row)
                row' = map (*a) row

backsubst :: Matrix -> [Rational]
backsubst rs = backsubst' rs []
    where
        backsubst' [] ps = ps
        backsubst' rs ps = backsubst' rs' (p:ps)
            where
                a   = (last rs) !! ((cols rs) - 2)
                c   = (last rs) !! ((cols rs) - 1)
                p   = c % a
                rs' = eliminate p (init rs)
                
solveSeq :: [Integer] -> [Rational]
solveSeq = backsubst . echelon . genMatrix                
                
ex92c = backsubst ex92b
ex92d = solveSeq [0,1,5,14,30]

{-
    The solution to [0,1,5,14,30] is [0 % 1,1 % 6,1 % 2,1 % 3]
    which gives the form:
    
        1/3*n^3 + 1/2*n^2 + 1/6*n + 0/1
     =  2n^3 + 3n^2 + n 
        --------------- 
               6        
     =  n(n+1)(2n+1)
        ------------
              6

-}
-- check that the above produces the original sequence
-- (Note: doesn't roung values)
ex92e = map (\ n -> (1/3)*n^3 + (1/2)*n^2 + (1/6)*n) [0..4]

{-
    The solution to [0,1,9,36,100,225] (sequence of sum of cubes)
    is [0 % 1,0 % 1,1 % 4,1 % 2,1 % 4] which gives the form:
    
        1/4*n^4 + 1/2*n^3 + 1/4*n^2 + 0/1*n + 0/1
      = n^4 + n^3 + n^2
        ---------------
                4
      = n^2(n+1)^2
        ----------
            4
      = ( n(n+1)/2 )^2
-}
ex92f = solveSeq [0,1,9,36,100,225]
ex92g = map (\n -> (1/4)*n^4 + (1/2)*n^3 + (1/4)*n^2) [0..5]
   
{-
    And the solution to [-12,-11,6,45,112,213,354,541,780,1077]
    is [(-12) % 1,(-5) % 1,5 % 1,1 % 1] which gives us the form:
    
        n^3 + 5*n^2 - 5*n - 12
-}   
ex92h = solveSeq [-12,-11,6,45,112,213,354,541,780,1077]
ex92i = map (\n -> n^3 + 5*n^2 - 5*n - 12) [0..9]

{-
    From the forgoing, we can easily see that we are representing
    the polynomial functions as lists of their constant coefficients.
    We can easily write a function to use this representation.
    The 'p2fct' function in Polynomials.hs does just that
-}
ex92j = map (p2fct [-12,-5,5,1]) [0..9]

-- here is the automated solution to Exercise 9.3
ex92k = solveSeq [3,7,17,39,79]     -- [3,3,0,1] -> n^3 + 3n + 3

{-
    Note: from provided solution to Exercise 9.6
    
    If you arrive at a solution by solving a set of linear equations
    derived from a polynomial sequence there is no need for an
    inductive proof as no guess work is involved.
-}
{-
    9.3 Polynomials and the Binomial Theorem
    ----------------------------------------
    
            n!          Gives the number of k-sized subsets that can
        ---------       be chosen from a set of size n (order does
        k! (n-k)!       not matter)
    
        Ex: n choose 0 = 1
            n choose 1 = n
            n choose 2 = n(n-1)/2
            n choose 3 = n(n-1)(n-2)/6
            
    [Note: text has proofs for Newtons Binomial Theorem and the
           following functions]
            
-}
-- number of k-sized subsets that can be chosen from a set of size n
choose n k = (product [n-k+1..n]) `div` (product [1..k])

{-
    Pascal's Triangle
    -----------------
    A binomial is the sum of two terms (x+y) so the binomial
    theorem gives us
    
        (x+y)^0 = 1x^0y^0
        (x+y)^1 = 1x^1y^0 + 1x^0y^1
        (x+y)^2 = 1x^2y^0 + 2x^1y^1 + 1x^0y^2
        (x+y)^3 = 1x^3y^0 + 3x^2y^1 + 3x^1y^2 + 1x^0y^3
        ...
        
    To see how this happens, look at what happens when we raise
    (x+y) to the nth power:  (x+y)(x+y)...(x+y)
    
                                            x + y
                                            x + y *
                              -------------------
                              x^2 + xy + xy + y^2
                                            x + y *
    ---------------------------------------------                                            
     x^3 + x^2y + x^2y + xy^2 + xy^2 + xy^2 + y^3
                                            x + y *
    ---------------------------------------------
                                           etc...    
                                           
    Every term in the expansion is a product of x-factors and
    y-factors, with the total number of factors always being
    n, so each term has the form x^ky^(n-k). We can arrange
    binomial coefficients in a triangle that works out to  
    Pascal's triangle

                            0
                            0
                        1       1
                        0       0
                    2       2       2
                    0       1       2
                3       3       3      3
                0       1       2       3
                
                           etc
                           
    and working out their values i.e. n choose k, gives
            
                            1
                        1       1
                      1     2     1
                    1   3      3    1
                  1   4     6     4    1
                  
                           etc
                           
    and that gives the pattern:
    
                n   n-1     n-1
                  =      +
                k   k-1      k
                        
    which reduces to 
                        n!
                    ---------               [see choose' fn below]
                    k! (n-k)!
                    
    while a closer look reveals the follwoing symmetry
    
                    n        n
                       =                    [see binom fn below]
                    k       n-k

-}
-- alternative definition, not as efficient as the first as repeatedly
-- computes intermediate values
choose' n 0 = 1
choose' n k | n < k  = 0
            | n == k = 1
            | otherwise =  choose' (n-1) (k-1) 
                        + (choose' (n-1) (k))
 
-- an even better (efficient) choose function
binom n 0 = 1
binom n k | n < k = 0
          | otherwise = (n * binom (n-1) (k-1)) `div` k
          
{-
    9.4 Polynomials for Combinatorial Reasoning
    -------------------------------------------
   
    To implement polynomial functions in a variable z we will
    represent a polynomial
    
        f(z) = f0 + f1z + f2z^2 + ... + f(n-1)z^(n-1) + fnz^n
        
    and a list of its coefficients as
    
        [f0, f1, ..., fn]
        
    the function 'p2fct' maps the coeff list to corresponding
    functions.
    
    f(z) = 0 is the constant zero polynomial
    If f(z) is a polynomial, then we f for its coeff list
    If the coeff list is not null, then the tail of the list is f_
    so
        f  = [f0, f1, ... , fn]
        f_ = [f1, f2, ... , fn]
        
    which gives the identity:  f = f0:f_
    and:  f(z) = f0 + zf_(z)
    
    The 'Polynomials.hs' file provides a number of functions for
    working with polynomials which are declared as a data type
    in class Num.
    
        negate          negate all coeffs in the polynomial
        addition        add the coeffs of both polynomials
        multiply (.*)   shift all coeffs one place to the right
   
-}          
ex94a = (z+1)^0                 -- [1]
ex94b = (z+1)                   -- [1,1]
ex94c = (z+1)^2                 -- [1,2,1]
ex94d = (z+1)^3                 -- [1,3,3,1]
ex94e = (z+1)^4                 -- [1,4,6,4,1]
ex94f = (z+1)^5                 -- [1,5,10,10,5,1]
ex94g = (z+1)^6                 -- [1,6,15,20,15,6,1]

{-
    If we have a polynomial f(z) and we want the difference
    list of its coeff [f0, f1-f0, f2-f1,...] then
        
        f(z) -> [ f0,   f1,   f2,   f3, ... ]
      -zf(z) -> [  0,  -f0,  -f1,  -f2, ... ]
   (1-z)f(x) -> [ f0,f1-f0,f2-f1,f3-f2, ... ]
   
   The Polynomial.hs file defines the function 'delta' which
   computes the difference list of the coefficients.
   
   [Note:  this is NOT the same as the earlier difference functions
           where we mapped a function to a list and took the 
           difference between the results]
           
    Example:
            delta [2,4,6]
            
            [  2,  4,  6,  0  ]
          - [  0,  2,  4,  6  ]
          = [  2,  2,  2, -6  ]
-}
ex94h = delta [2,4,6]           -- [2,2,2,-6]

{-
    Polynomial composition
    
    comp    f(z) compose g(z) is the polynomial f(g(z))
            which equals f0 + g(z) * f_(g(z))

    We can use composition to pick an arbitrary layer in Pascal's
    Triangle or to generate Pascal's triangle up to an arbitrary
    depth.
    
    A derivative function, deriv, is also provided.

-}
ex94i = comp (z^2)  (z+1)            -- [1,2,1]
ex94j = comp (z^3)  (z+1)            -- [1,3,3,1]
ex94k = comp (z^12) (z+1) -- [1,12,66,220,495,792,924,792,495,220,66,12,1]

ex94l = comp [1,1,1,1,1,1] [[0],[1,1]]
    -- [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]

{-
    9.17 Examples
    
    How many ways are there of selecting ten red, blue or white 
    marbles fro a vase in such a way that there are at least
    two of each coloar and at most five marbles have the same
    color?
    
    The answer is given by the coefficient z^10 in the following
    polynomial:
            (z^2 + z^3 + z^4 + z^5)^3
-}    
ex917a = ([0,0,1,1,1,1]^3) !! 10        -- 12

{-
    How many ways are there of selecting 10  red, blue or
    white marbles from a vase ins such a way that there is
    an even number of marbles of each color?
-}
ex917b = ([1,0,1,0,1,0,1,0,1,0,1]^3) !! 10       -- 21
  
{-
    9.18 Example
    
    The polynomial (1+z)^10 solves the problem of picking r
    elements from a set of 10. The finite list that solves
    the problem,
        [1,10,45,120,210,252,210,120,45,10,1]
    is implement below
-}  
ex918 = (1+z)^10

{-
    9.19 Example
    
    The polynomial for picking r marbles from a vase containing
    red, white and blue marbles, with a maximum of 5 of each 
    color is:
        (1 + z + z^2 + z^3 + z^4 + z^5)^3
        
    The solution 
        [1,3,6,10,15,21,25,27,27,25,21,15,10,6,3,1]
    is given by
-}
ex919 = (1 + z + z^2 + z^3 + z^4 + z^5)^3

{-
    9.20 Exercise
    
    Use polynomials to find out how many ways there are of
    selecting 10 red, blue and white marles from a vase
    in such a manner that the number of marbles from each
    color is a prime.
    
    Provide solution:
        put a 1 on the positions for the prime exponents
-}
ex920 = ([0,0,1,1,0,1,0,1,0,0,0]^3) !! 10           -- 6

{-
    9.5 Addendum
    
    Contains the following functions which provide an
    alternative to Gaussian Elimination for curve-fitting
    based on the use of 'falling' and 'rising' factorials
    used by Newton (?)

-}
-- find the falling power or lower factorial
infixr 8 ^-

(^-) :: Integral a => a -> a -> a
x ^- 0 = 1
x ^- n = (x ^- (n-1)) * (x - n + 1)

-- find the rising powers or upper factorials
infixr 8 ^+

(^+) :: Integral a => a -> a -> a
x ^+ 0 = 1
x ^+ n = (x ^+ (n-1)) * (x + n - 1)

-- Newton's series formula
newton :: (Fractional a, Enum a) => [a] -> [a]
newton xs = [ x / product [1..fromInteger k]  |
              (x,k) <- zip xs [0..]]
              
-- mapping a list of integers to a Newton polynomial representation
-- (list of the exponents, with the exp representing falling powers)
list2npol :: [Integer] -> [Rational]
list2npol = newton . map fromInteger . firstDifs

-- compute Stirling numbers
stirlingC :: Integer -> Integer -> Integer
stirlingC 0 0 = 1
stirlingC 0 _ = 0
stirlingC n k = (n-1) * (stirlingC (n-1) k) + stirlingC (n-1) (k-1)

-- convert falling power to standard powers
fall2pol :: Integer -> [Integer]
fall2pol 0 = [1]
fall2pol n = 0 : [(stirlingC n k) * (-1)^(n-k) | k <- [1..n]]

-- convert Newton polynomials to standard polynomials
-- (in coeff representation)
npol2pol :: (Num a, Ord a) => [a] -> [a]
npol2pol xs = sum [ [x] * (map fromInteger $ fall2pol k) |
                              (x,k) <- zip xs [0..]]
                              
-- compute a polynomial from a sequence
list2pol :: [Integer] -> [Rational]
list2pol = npol2pol . list2npol

ex95a = list2pol (map (\n -> 7*n^2 + 3*n-4) [0..100])
ex95b = list2pol [0,1,5,14,30]
ex95c = map (p2fct $ list2pol [0,1,5,14,30]) [0..8]   

                           
                              
