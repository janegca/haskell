{-
    Chapter 4 - Sets, Types and Lists
    
    This module display levels of the 'Set Theoretic Universe'
    
    Number of pairs of braces for each set expansion:
        2(N-1)(P-1) 
        
    where N is the number of elements in a set
          P is the number of brace {} pairs
          
    1.  1 new pair of outermost braces
    2.  for each 2^N elements, 1 pair of outermost braces
    3.  each of the N elements of A occurs in half of the elements of 
        set(A), i.e., in 2^(N−1) elements of set(A); for
        these we need P - 1 brace pairs (all brace pairs of A, minus the
        outermost brace pair); all in all this gives 2^(N−1)(P - 1) brace 
        pairs  
          
    Set     # of elements       # brace pairs
    ---     -------------       -------------
    v0              0               1
    v1              1               2
    v2              2               4
    v3              4              11
    v4             16               1 + 16 + 8 × 10 = 97
    v5          65536               1 + 2^16 + 2^15 × 96 = 3211265
        
    Using  zero (0) for the empty set reduces the required number
    of braces for v5 to 1867776.
    
    File source: http://homepages.cwi.nl/~jve/HR/#Solutions
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
       
-}
-- used to display hierarchies of empty sets
module Hierarchy where 

--import STAL(display)
import SetEq 

data S = Void deriving (Eq,Show)
empty :: Set S
empty = Set []

v0 = empty 
v1 = powerSet v0
v2 = powerSet v1 
v3 = powerSet v2
v4 = powerSet v3 
v5 = powerSet v4

display :: Int -> String -> IO ()
display n str = putStrLn (display' n 0 str)
  where 
  display' _ _ [] = []
  display' n m (x:xs) | n == m    = '\n': display'  n   0  (x:xs)
                      | otherwise =  x  : display'  n (m+1)  xs 

-- example usage
ex1 = v5 !!! 0
ex2 = v5 !!! 1
ex3 = v5 !!! 2
ex4 = v5 !!! 3



