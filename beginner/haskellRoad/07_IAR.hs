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

{-
    7.4 Induction and Recursion over Trees
    --------------------------------------
    A single leaf is a binary tree of depth 0
    
    If t1 and t2 are binary trees, then the result of joining
    t1 and t2 under a single root is (leaf t1 t2) with a depth 
    of 1 + the maximum of the depth of t1 and t2.
    
    In general, the number of nodes in a balanced tree of depth
    n is 2^(n+1) - 1.
    
    Basic tree definitions in Haskell are given below
-}
data BinTree = L | N BinTree BinTree deriving Show

makeBinTree :: Integer -> BinTree
makeBinTree 0 = L
makeBinTree n = N (makeBinTree (n-1)) (makeBinTree (n-1))

count :: BinTree -> Integer
count L         = 1
count (N t1 t2) = 1 + count t1 + count t2

depth :: BinTree -> Integer
depth L = 0
depth (N t1 t2) = (max (depth t1) (depth t2)) + 1

balanced :: BinTree -> Bool
balanced L = True
balanced (N t1 t2) = (balanced t1) && (balanced t2) 
                  && depth t1 == depth t2
                  
{-
    Exercise 7.25
    
    Write a Haskell definition of ternary trees, plus procedures
    for generating balanced ternary trees and counting their 
    nodes.
    
    Note: total nodes in a balanced ternary tree should equal
          (3^(n+1) - 1)/2
-}        
data TTree = TL | TN TTree TTree TTree deriving Show

makeTTree :: Integer -> TTree
makeTTree 0 = TL
makeTTree n = TN (makeTTree (n-1)) (makeTTree (n-1)) (makeTTree (n-1))

count' :: TTree -> Integer
count' TL            = 1
count' (TN t1 t2 t3) = 1 + count' t1 + count' t2 + count' t3

depth' :: TTree -> Integer
depth' TL            = 0
depth' (TN t1 t2 t3) = let dt1 = depth' t1
                           dt2 = depth' t2
                           dt3 = depth' t3
                       in 1 + max (max dt1 dt2) dt3

balanced' :: TTree -> Bool
balanced' TL = True
balanced' (TN t1 t2 t3) =
       balanced' t1 && balanced' t2 && balanced' t3 
    && (depth' t1 == depth' t2) && (depth' t2 == depth' t3)

{-
   If we define a tree with integer numbers in the at the
   internal nodes, the tree is 'ordered' if, for every node
   N, the integer numbers in the left tree are less than
   N and the integer numbers in the right tree are greater
   than N.

-}    
data Tree = Lf | Nd Int Tree Tree deriving Show

{-
    Exercise 7.28
    
    Write a function that inserts a number n in an ordered tree
    in such a way that the tree remains ordered.
-}      
insertTree :: Int -> Tree -> Tree
insertTree n Lf = Nd n Lf Lf
insertTree n t@(Nd x t1 t2) 
    | n < x     = Nd x (insertTree n t1) t2
    | n > x     = Nd x t1 (insertTree n t2)
    | otherwise = t

{-
    Exercise 7.29
    
    Write a function 'list2tree' that converts a list of
    integers to an ordered tree, with the integers at
    the tree nodes. The type is [Int] -> Tree. Also,
    write a function 'tree2list' for conversion
    in the other direction.

-}
list2tree :: [Int] -> Tree
list2tree []     = Lf
list2tree (x:xs) = insertTree x (list2tree xs)

tree2list :: Tree -> [Int]
tree2list Lf           = []
tree2list (Nd n t1 t2) = (tree2list t1) ++ n : (tree2list t2)

{-
    Exercise 7.30
    
    Write a function that checks whether a given integer i
    occurs in an ordered tree.

-}
inTree :: Int -> Tree -> Bool
inTree n Lf           = False
inTree n (Nd x t1 t2) | n == x    = True
                      | n < x     = inTree n t1
                      | otherwise = inTree n t2
                      
{-
    Exercise 7.31
    
    Write a function that merges two ordered trees into a
    new tree containing all the numbers of the input trees.
-}                      
mergeTrees :: Tree -> Tree -> Tree
mergeTrees t1 Lf = t1
mergeTrees Lf t2 = t2
mergeTrees t1@(Nd x a b) t2@(Nd y c d) 
    | x == y = Nd x (mergeTrees a c) (mergeTrees b d)
    | x < y  = Nd y (mergeTrees t1 c) d
    | otherwise = Nd x a (mergeTrees b t2)                      

-- provided solution
mergeTrees' :: Tree -> Tree -> Tree
mergeTrees' t1 t2 = foldr insertTree t2 (tree2list t1)    

{-
    Exercise 7.32
    
    Write a function that counts the number of steps that
    are needed to reach a number i in an ordered tree.
    The function should give 0 if i at the top node,
    -1 if i does not occur in the tree at all.
-}
stepsToN :: Int -> Tree -> Int
stepsToN n Lf   = -1
stepsToN n t@(Nd x t1 t2) =
    if found then steps else -1
    where  -- inefficient, going thru tree twice
        found = inTree n t        
        
        steps | n == x    = 0
              | n < x     = 1 + stepsToN n t1
              | otherwise = 1 + stepsToN n t2
              
-- provided solution
findDepth :: Int -> Tree -> Int
findDepth _ Lf = -1
findDepth n (Nd m left right)
    | n == m = 0
    | n < m  = if d1 == -1 then -1 else d1 + 1
    | n > m  = if d2 == -1 then -1 else d2 + 1
    where d1 = findDepth n left
          d2 = findDepth n right              
              
{-
    Following is a general data type for trees with information
    at the internal nodes
-}              

data Tr a = Nil | T a (Tr a) (Tr a) deriving (Eq, Show)

insertT :: (Ord a) => a -> Tr a -> Tr a
insertT n Nil = T n Nil Nil
insertT n t@(T x t1 t2) 
    | n < x     = T x (insertT n t1) t2
    | n > x     = T x t1 (insertT n t2)
    | otherwise = t

list2T :: Ord a => [a] -> Tr a
list2T []     = Nil
list2T (x:xs) = insertT x (list2T xs)
  
{-
    Exercise 7.33
    
    Write a function
        mapT :: (a -> b) -> Tr a -> Tr b
        
    that does for binary trees what 'map' does for lists.
-}
mapT :: (a -> b) -> Tr a -> Tr b
mapT f Nil = Nil
mapT f (T x t1 t2) = T (f x) (mapT f t1) (mapT f t2)

ex733 = let t = list2T [1..10] in mapT (*2) t
                      
{-
    Exercise 7.34
    
    Write a function
        foldT :: (a -> b -> b -> b) -> b -> (Tr a) -> b
        
    that does for binary trees what foldn does for naturals.
-}
-- provided solution
foldT :: (a -> b -> b -> b) -> b -> (Tr a) -> b
foldT h c Nil = c
foldT h c (T x left right) = h x (foldT h c left) (foldT h c right)
    
{-
    Exercise 7.35
    
    Conversion of a tree to a list can be done in various ways
    depending on when the node is visited.
    
    Preorder Traversal
        first visit the node, followed by the left subtree and then
        the right subtree

    Inorder Traversal
        visit the left subtree, the node and then the right subtree
        
    Postorder Traversal
        visit the left subtree, the right subtree and then the node
        
    Define these three conversion functions from trees to lists
    in terms of the foldT function in exercise 7.34
-}
-- provided solution  
preorderT, inorderT, postorderT :: Tr a -> [a]
preorderT = foldT preLists []
    where preLists x ys zs = (x:ys) ++ zs

inorderT = foldT inLists []
    where inLists x ys zs = ys ++ [x] ++ zs

postorderT = foldT postLists []
    where postLists x ys zs = ys ++ zs ++ [x]    

-- how does this work?
tr1 = list2T [1..10]             -- example tree
ex735a = preorderT  tr1          -- gives list in reverse order
ex735b = inorderT   tr1          -- gives list in order
ex735c = postorderT tr1          -- gives list in order

{-
    Exercise 7.36
    
    An ordered tree is a tree with information at the nodes
    given in such manner that the item at a node must be bigger
    than all items in the left subtree and smaller than all items
    in the right subtree. A tree is ordered iff the list resulting
    from its inorder traversal is ordered and contains no
    duplicates. Give an implementation of this check.
    
-}
isInOrder :: Ord a => Tr a -> Bool
isInOrder tree = let lst = inorderT tree
                 in  lst == (nub $ sort lst)
                 
-- provided solution
orderedT :: Ord a => Tr a -> Bool
orderedT tree = ordered (inorderT tree)
    where
        ordered xs = (sort (nub xs) == xs)        

{-
    Exercise 7.37
    
    An ordered tree (Exercise 7.36) can be used as a dictionary
    by putting items of type (String, String) at the internal
    nodes, and defining the ordering as:
    
        (v,w) <= (v',w') iff v <= v'
        
    Give the code for looking up a word definition in a 
    dictionary (data type defined below).

-}        
type Dict = Tr (String, String)

lookupD :: String -> Dict -> [String]
lookupD key Nil = []
lookupD key (T (k,d) t1 t2)
    | key == k  = [d]
    | key <= k  = lookupD key t1
    | otherwise = lookupD key t2
    
{-
    Exercise 7.38
    
    For efficient search in an ordered tree, it is crucial that
    the tree is balanced: the left and right subtrees should 
    have nearly the same depth and should themselves be balanced.
    
    The auxiliary function 'split' (given below) splits a
    non-empty list into roughly equal lengths. Use this to
    implement a function 
    
        buildTree :: [a] -> Tr a
        
    for transforming an ordered list into an ordered and
    balanced binary tree.

-}    
split :: [a] -> ([a],a,[a])
split xs = (ys1, y, ys2)
    where
        ys1     = take n xs
        (y:ys2) = drop n xs
        n       = length xs `div` 2

-- provided solution        
buildTree :: [a] -> Tr a
buildTree [] = Nil
buildTree xs = T m (buildTree left) (buildTree right)
    where (left,m,right) = split xs

{-
    Following are definitions for a binary leaf tree (binary
    trees with information at the leaf nodes rather than
    the internal nodes)
-}    
data LeafTree a = Leaf a
                | Node (LeafTree a) (LeafTree a)
    deriving Show
    
-- an eample leaf tree
ltree :: LeafTree String
ltree = Node
          (Leaf "I")
          (Node 
            (Leaf "love")
            (Leaf "you"))
            
{-
    Exercise 7.39
    
    Repeat exercise 7.33 for leaf trees. Call the new map
    function mapLT.
-}   
mapLT :: (a -> b) -> LeafTree a -> LeafTree b
mapLT f (Leaf x)     = (Leaf (f x))
mapLT f (Node t1 t2) = Node (mapLT f t1) (mapLT f t2)

{-
    Exercise 7.40
    
    Give code for mirroring a leaf tree on its vertical axis.
    Call the function 'reflect'. In the mirroring process,
    the left- and right branches are swapped, and the same
    swap takes place recursively within the branches.
    The reflection of
        Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    is
        Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)
-}        
lt1 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
lt2 = Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)

reflect :: LeafTree a -> LeafTree a
reflect (Leaf x)     = Leaf x
reflect (Node t1 t2) = Node (reflect t2) (reflect t1)
   
{-
    A data type for trees with arbitrary numbers of branches
    (rose tree), with information of type a at the buds is
    given below along with an example rose

-}   
data Rose a = Bud a | Br [Rose a] deriving (Eq, Show)

rose = Br [Bud 1, 
           Br [Bud 2, Bud 3,
               Br [Bud 4, Bud 5, Bud 6]]]
               
{-
    Exercise 7.42
    
    Write a function 
        mapR :: (a -> b) -> Rose a -> Rose b
    
    that does for rose trees what map does for lists. For the
    example rose, we should get:
    
        mapR succ rose
            Br [Bud 2, Br [Bud 3, Bud 4, Br [Bud 5, Bud 6, Bud 7]]]
-}             
-- provided solution 
mapR :: (a -> b) -> Rose a -> Rose b
mapR f (Bud a)    = Bud (f a)
mapR f (Br roses) = Br (map (mapR f) roses)

{-
    7.5 Induction and Recursion over Lists
    --------------------------------------
    Induction and recursion over lists is based on two cases: [], (x:xs)
    
    A general scheme for structural recursion over lists (without
    extra parameters) is:
            f []    := z
            f(x:xs) := h x (f xs)
            
    For example, the function 's' computes the sum of a list of 
    numbers is defined by:
    
                       s [] := 0
                    s(n:xs) := n +  s xs
    
        where the zero is taken for 'z' and '+' for 'h'
        
    This general case is defined by as 'foldr' in Haskell.
    
        foldr : (a -> b -> b) -> b -> [a] -> b
        foldr f z []     = z
        foldr f z (x:xs) = f x (foldr f z xs)
        
        where, in general, 'z' is the identity element of the
               operation (eg '0' for addition, '1' for multiplication, etc)
               
    Informally, the definition of 'foldr' is
    
        foldr (op) z [x1,x2,...,xn] := x1 op(x2 op(...(xn op z)...))
        
    We can redefine the functions for Natural numbers using 'foldr'
    
-}
-- example of predefined recursive operations over lists
len :: [a] -> Int                   -- length in Prelude
len []     = 0                      -- base case
len (x:xs) = 1 + len xs             -- recursive case
     
cat :: [a] -> [a] -> [a]            -- (++) in Prelude
cat [] ys     = ys                  -- base case
cat (x:xs) ys = x : (cat xs ys)     -- recursive case

-- Natural number operations defined using 'foldr'
add :: [Natural] -> Natural
add = foldr plus Z

mlt :: [Natural] -> Natural
mlt = foldr mult (S Z)

-- an alternative length definition for natural numbers
-- returns the list length as a Natural number
ln :: [a] -> Natural
ln = foldr (\ _ n -> S n) Z

{-
    Exercise 7.46
    
    Use foldr to give a new implementation of generalized
    union and foldr1 to give a new implementation of 
    generalized intersection for lists (Look up the code for
    foldr1 in the Haskell prelude. Compare with exercise 4.53).
    
    Source code for 'foldr1' from Prelude:
    
        foldr1           :: (a -> a -> a) -> [a] -> a
        foldr1 _ [x]     =  x
        foldr1 f (x:xs)  =  f x (foldr1 f xs)
        foldr1 _ []      =  errorEmptyList "foldr1"    
        
    Code from Ex 4.53
    
        genUnion :: Eq a => [[a]] -> [a]
        genUnion []       = []
        genUnion [xs]     = xs
        genUnion (xs:xss) = union xs  (genUnion xss)

        genIntersect :: Eq a => [[a]] -> [a]
        genIntersect []       = error "list of lists should be non-empty"
        genIntersect [xs]     = xs
        genIntersect (xs:xss) = intersect xs (genIntersect xss)    
-}

-- provided solutions
genUnion :: Eq a => [[a]] -> [a]
genUnion = foldr union []

genIntersect :: Eq a => [[a]] -> [a]
genIntersect = foldr1 intersect

{- 
    Consider the following are definitions of generalized conjunction and 
    disjunction
    
        or :: [Bool] -> Bool
        or []     = False
        or (x:xs) = x || or xs

        and :: [Bool] -> Bool
        and []     = True
        and (x:xs) = x && and xs
        
    To generalize these functions using 'foldr' we only need to
    know the identity function for 'or' (True) and 'and' (False)
    and so the defintions in Prelude.hs are
    
        and, or :: [Bool] -> Bool
        and = foldr (&&) True
        or  = foldr (||) False
-}

{-
    Exercise 7.47
    
    Define a function, srt, that sorts a list of items in class
    Ord a by folding the list with a function insrt.
-}
srt :: Ord a => [a] -> [a]
srt = foldr insrt []
    where
        insrt :: Ord a => a -> [a] -> [a]
        insrt x [] = [x]
        insrt x (y:ys) | x < y = x : y : ys
                       | otherwise = y : insrt x ys
{-
    foldl
        where 'foldr' folds from the right, 'foldl' folds from 
        the left
        
            foldl :: (a -> b -> a) -> a -> [b] -> a
            foldl f z []     = z
            foldl f z (x:xs) = foldl f (f z x) xs
            
    An informal defintion of foldl is
    
        foldl (op) z [x1,x2,...,xn] := (...((z op x1)opx2)op...)op xn
        
    Which can be used to pick out the following recursio theme
    
                f z []     :=  z
                f z (x:xs) := f (h z x) xs

    which boils down to recursion over lists with an extra
    parameter
                     r zs [] := zs
                 r zs (x:xs) := r (prefix xs x) xs
                 
            where 'prefix' is given by prefix ys y = y:ys
            
    Example using 'rev' (below)
    
            rev [1,2,3]
         -> foldl (\ xs x -> x:xs) [] [1,2,3]
         -> foldl (\ xs x -> x:xs) ((\ xs x -> x:xs) [] 1)[2,3]
         -> foldl (\ xs x -> x:xs) [1] [2,3]
         -> foldl (\ xs x -> x:xs) ((\ xs x -> x:xs) [] 2) [3]
         -> foldl (\ xs x -> x:xs) [2,1] [3]
         -> foldl (\ xs x -> x:xs) ((\ xs x -> x:xs) [] 3) []
         -> foldl (\ xs x -> x:xs) [3,2,1] []
         -> [3,2,1]
         
    A 'foldr' version would be less efficient as it would require
    an additional (++) operation (see rev' below) which is also
    recursive. Assuming 'postfix' = (\ x xs -> xs ++ [x]), a
    walkthrough of reverse using 'foldr' would look like the
    following
    
            rev' [1,2,3]
         -> foldr postfix [] [1,2,3]
         -> postfix 1 (foldr postfix [] [2,3])
         -> (foldr postfix [] [2,3]) ++ [1]
         -> (postfix 2 (foldr postfix [] [3])) ++ [1]
         -> (foldr postfix [] [3]) ++ [2] ++ [1]
         -> (postfix 3 (foldr postfix [] [])) ++ [2] ++ [1]
         -> (foldr postfix [] []) ++ [3] ++ [2] ++ [2]
         -> [] ++ [3] ++ [2] ++ [1]
         -> ([3] ++ [2]) ++ [1]
         -> 3 : ([] ++ [2] [1]
         -> [3,2] ++ [1]
         -> 3 : ([2] ++ [1])
         -> 3 : 2 : ([] ++ [1])
         -> [3,2,1]
    
    It's easy to see the two folding operations h and h' are 
    similar
    
               f [] := z                       f' [] := z
            f(x:xs) := h x (f xs)           f'(x:xs) := h' (f' xs) x
            
    In the examples above
        postfix x [] = [] ++ [x] = [x] = prefix [] x
        
    and
        postfix x (prefix xs y) 
      = (prefix x y) ++ [x]
      = y:(xs + [x])
      = y:(postfix x xs)
      = prefix (postfix x xs) y
                    
                
-}
-- reverse in terms of 'foldl'
rev :: [a] -> [a]
rev = foldl (\ xs x -> x:xs) []

-- less efficient reverse in terms of 'foldr'
rev' :: [a] -> [a]
rev' = foldr (\ x xs -> xs ++ [x]) []

{-
    Exercise 7.50
    
    Consider the version of reverse below. Which version is
    more efficient, the original rev, the version rev' or
    this one? Why?
    
    Ans: rev and rev2 have the same efficiency; same operation
         and both are better than rev'

-}
rev1 :: [a] -> [a]
rev1 xs = rev2 xs []
    where
        rev2 []     ys = ys
        rev2 (x:xs) ys = rev2 xs (x:ys)
        
{-
    Exercise 7.51
    
    Define an alternative version of ln using foldl.
-} 
-- provided solution
{-
        ln' [n1,n2]
     -> ln' [S Z, S(S Z)]
     -> foldl (\ n _ -> S n) Z [S Z, S(S Z)]
     -> foldl (\ n _ -> S n) ((\ n _ -> S n) Z S Z) [S(S Z)]
     -> foldl (\ n _ -> S n) (S Z) [S(S Z)]
     -> foldl (\ n _ -> S n) ((\ n _ -> S n) (S Z) S(S Z)) []
     -> foldl (\ n _ -> S n) S(S Z) []
     -> S(S Z)
-}
ln' :: [a] -> Natural
ln' = foldl (\ n _ -> S n) Z

{-
    The list operations, map and filter, can be combined
    
    exFilterMap below produces [5,6,7,8,9,10,11]
    exMapFilter produces [6,7,8,9,10,11]
    
    because in the first example, the filter is applied 'after'
    the mapping:
           filter (>4) (map (+1) [1,2,3,4,5,6,7,8,9,10])
        -> filter (>4) [2,3,4,5,6,7,8,9,10,11]
        -> [5,6,7,8,9,10,11]
        
    while in the second example, the filter is applied 'before'
    the mapping:
           map (+1) (filter (>4) [1,2,3,4,5,6,7,8,9,10])
        -> map (+1) [5,6,7,8,9,10]
        -> [6,7,8,9,10,11]
    
    the third example corrects the filtering in the second
    example, adding 1 to the number and then checking if it's
    greater than 4
            map (+1) (filter ((>4) . (+1)) [1..10])
         -> map (+1) [4,5,6,7,8,9,10]
         -> [5,6,7,8,9,10,11]
         
    The composition ((>4) . (+1)) is equivalent to (>3)
    showing
    
        filter p (map f xs) = map f (filter (p . f) xs)
    
-}
exFilterMap  = filter (>4) (map (+1) [1..10])
exMapFilter  = map (+1) (filter (>4) [1..10])
exMapFiltera = map (+1) (filter ((>4) . (+1)) [1..10])

{-
    7.6 Variations on the Tower of Hanoi
    -----------------------------------
    Basic Tower of Hanoi
        3 pegs, 8 discs stacked in descending size on the 1st peg
        Object is to transfer the stack to the 2nd peg while
        keeping to the following rules:
            1. move only one disc at a time
            2. never place a larger disc on top of a smaller one
            
        The least number of moves is 2^n - 1, with 8 disks
        that's 2^8 - 1 = 255 moves.
        
    An obvious way to represent the configuration is:
        ([1,2,3,4,5,6,7,8],[],[])
    with the pegs named A, B and C
    
    There are six possible moves from one peg to another.
    
    We can write a 'transfer' operation to move the disks
    creating a new configuration

-}
data Peg = A | B | C
type Tower = ([Int],[Int],[Int])

move :: Peg -> Peg -> Tower -> Tower
move A B (x:xs,   ys,   zs) = (  xs, x:ys,   zs)
move B A (  xs, y:ys,   zs) = (y:xs,   ys,   zs)
move A C (x:xs,   ys,   zs) = (  xs,   ys, x:zs)
move C A (  xs,   ys, z:zs) = (z:xs,   ys,   zs)
move B C (  xs, y:ys,   zs) = (  xs,   ys, y:zs)
move C B (  xs,   ys, z:zs) = (  xs, z:ys,   zs)

transfer :: Peg -> Peg -> Peg -> Int -> Tower -> [Tower]
transfer _ _ _ 0 tower = [tower]
transfer p q r n tower = transfer p r q (n-1) tower
                      ++ transfer r q p (n-1) (move p q tower')
    where tower' = last (transfer p r q (n-1) tower)
    
hanoi :: Int -> [Tower]
hanoi n = transfer A B C n ([1..n], [], [])

{-
    Not every move in the above is correct; there are 3^n ways
    to stack n disks of decreasing size on 3 pegs so that no
    disk is put on a smaller disk. We can implement a procedure
    to only make valid moves
-}
check :: Int -> Tower -> Bool
check 0 t = t == ([],[],[])
check n (xs, ys, zs)
    | xs /= [] && last xs == n = check (n-1) (init xs, zs, ys)
    | zs /= [] && last zs == n = check (n-1) (ys, xs, init xs)
    | otherwise                = False
    
-- find the largest disk in the configuration
maxT :: Tower -> Int
maxT (xs, ys, zs) = foldr max 0 (xs ++ ys ++ zs)

-- check a configuration of any size
checkT :: Tower -> Bool
checkT t = check (maxT t) t

    
-- in the end, only two types of moves can be made,
-- move the smallest disk or move a disk other than the smallest    
parity :: Tower -> (Int, Int, Int)
parity (xs,ys,zs) = par (xs ++ [n+1], ys ++ [n], zs ++ [n+1])
    where 
        n                      = maxT (xs, ys, zs)
        par (x:xs, y:ys, z:zs) = (mod x 2, mod y 2, mod z 2)

target :: Tower -> Peg
target t@(xs,ys,zs) | parity t == (0,1,1) = A
                    | parity t == (1,0,1) = B
                    | parity t == (1,1,0) = C
                    
move1 :: Tower -> Tower
move1 t@(1:_,  ys,  zs) = move A (target t) t
move1 t@( xs, 1:_,  zs) = move B (target t) t
move1 t@( xs,  ys, 1:_) = move C (target t) t        

-- moving the middle disk
move2 :: Tower -> Tower
move2 t@(1:xs,  [],   zs) = move C B t
move2 t@(1:xs,  ys,   []) = move B C t
move2 t@(1:xs,  ys,   zs) = if ys < zs 
                            then move B C t else move C B t
move2 t@(  [],1:ys,   zs) = move C A t
move2 t@(  xs,1:ys,   []) = move A C t
move2 t@(  xs,1:ys,   zs) = if xs < zs
                            then move A C t else move C A t
move2 t@(  [],  ys, 1:zs) = move B A t
move2 t@(  xs,  [], 1:zs) = move A B t
move2 t@(  xs,  ys, 1:zs) = if xs < ys 
                            then move A B t else move B A t

-- check for completion
done :: Tower -> Bool
done ([],[],_)  = True
done (xs,ys,zs) = False    

-- transfers occurring by alternating between move1 and move2
-- as the last move must be a move1 we only need to check if
-- we're done after a move1  
transfer1, transfer2 :: Tower -> [Tower]
transfer1 t = t : transfer2 (move1 t)
transfer2 t = if done t 
              then [t] else t : transfer1 (move2 t)

-- new Hanoi procedure
hanoi' :: Int -> [Tower]
hanoi' n = transfer1 ([1..n],[],[])

-- can now call hanoi' 64 without running into a memory problems
-- still takes an eternity
zazen :: [Tower]
zazen = hanoi' 64

{- 
    Exercise 7.61
    
    Define and implement a total ordering on the list of all
    the correct configurations.

-}
-- provided solution
compareT :: Tower -> Tower -> [Ordering]
compareT t t' 
    | maxT t < maxT t' = [ LT | checkT t && checkT t' ]
    | maxT t > maxT t' = [ GT | checkT t && checkT t' ]
    | otherwise        = [ compare' t t' | checkT t && checkT t' ]
    
compare' :: Tower -> Tower -> Ordering
compare' ([],[],[]) ([],[],[]) = EQ
compare' t@(xs,ys,zs) t'@(xs',ys',zs')
    | firstStage t && firstStage t' =
        compare' (init xs, zs, ys) (init xs', zs', ys')
    | lastStage t && lastStage t' =
        compare' (ys, xs, init zs) (ys', xs', init zs')
    | firstStage t && lastStage t' = LT
    | lastStage t && firstStage t' = GT
    where
        firstStage (xs,ys,zs) = xs /= [] && last xs == maxT t
        lastStage t = not (firstStage t)    
              
-- find the k-th configuration
hanoiCount :: Int -> Integer -> Tower
hanoiCount n k 
    | k < 0         = error "argument negative"
    | k > 2^n - 1   = error "argument not in range"
    | k == 0        = ([1..n], [], [])
    | k == 2^n - 1  = ([], [], [1..n])
    | k < 2^(n-1)   = (xs ++ [n], zs, ys)
    | k >= 2^(n-1)  = (ys', xs', zs' ++ [n])
    where
        (xs,ys,zs) = hanoiCount (n-1) k
        (xs',ys',zs') = hanoiCount (n-1) (k - 2^(n-1))
        
-- form a bijection between the natural numbers and the
-- correct tower configurations
toTower :: Integer -> Tower
toTower n = hanoiCount k m
    where
        n' = fromInteger (n+1)
        k  = truncate (logBase 2 n')
        m  = truncate (n' - 2^k)
  
{-
    Exercise 7.61
    
    The function hanoiCount gives us yet another approach to the 
    tower transfer problem. Implement this as hanoi''::Int->[Tower]
-}
-- provided solution
hanoi'' :: Int -> [Tower]
hanoi'' n = [ hanoiCount n k | k <- [0..2^(toInteger n)-1] ]
        
{-
    Exercise 7.62
    
    Implement the function fromTower :: Tower -> Integer
    that is the inverse of toTower
    
    Provided Answer:
    
    The key to the implementation is the observation that the initial 
    configuration of a tower with disk size n is preceded in the 
    ordering of all tower configurations by 2n−1 configurations for 
    towers of smaller sizes, as is easily proved by induction.
-}        
fromTower :: Tower -> Integer
fromTower t = (2^n - 1) + (fromT t n) 
    where
        n = maxT t
        fromT (xs,ys,zs) k
            | xs == [1..k] = 0
            | elem k xs    = fromT (init xs, zs, ys) (k-1)
            | elem k zs    = 2^(k-1) + fromT (ys, xs, init zs) (k-1)
        
{-
    7.7 Other Data Structures
    -------------------------
    
    
-}