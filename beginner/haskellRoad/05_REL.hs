{-
    Chapter 5 - Relations
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
import Data.List
import SetOrd

{-
    5.1 Relations as Sets of Ordered Pairs
    --------------------------------------
    Relation        
        A relation, R, is a set of 'ordered pairs'
        Written as: xRy or R(x,y) or Rxy
    
    Domain          the set of all the first coordinates of pairs in R
    Range           the set of all the second coordinates of pairs in R
        
    Example:
        If R::AxB is an empty set then the domain and range of R are 
        both empty sets.
        
    The relation R is a relation 'from A to B' or 'between A and B'
    only if the domain of R is a subset of A and the range of R is
    a subset of B i.e. the first coord of each ordered pair is 
    drawn from set A and the second coord of each ordered pair is
    drawn from set B.
        
    Identity Relation
    -----------------
        {(a,b) in A | a = b} = {(a,a)| a in A}
        
    Inverse Relation
    ----------------
        The inverse of R::AxB is R'::BxA
        or R = {(a,b) | aRb} and R' = {(b,a) | aRb }          
-}
-- Example 5.11
--  A relation giving all the divisors of 'n'
--      {(a,b) | a,b in N, ab = n, a <= b}
--
divisors :: Integer -> [(Integer,Integer)]
divisors n = [ (d, quot n d) | d <- [1..k], rem n d == 0 ]
    where k = floor (sqrt (fromInteger n))
 
-- Example 5.12
--  Test for primality using the divisors function
--
prime :: Integer -> Bool
prime = \n -> divisors n == [(1,n)]

-- Find the divisors of the given natural number
divs :: Integer -> [Integer]
divs n = [ d | d <- [1..n], rem n d == 0 ]

-- Find the proper divisors of the given natural number
-- [A 'proper divisor' is any positive divisor of a number
--  excluding the number itself i.e. 1 is a proper divisor
--  of any prime, 1,2 and 3 are the proper divisors of 6.]
properDivs :: Integer -> [Integer]
properDivs n = init (divs n)

-- Is the given natural number a perfect natural number?
-- (A 'perfect number' is equal to the sum of its divisors
--  excluding itself i.e. 6 is equal to the sume of its
--  divisors: 1 + 2 + 3)
--
perfect :: Integer -> Bool
perfect n = sum (properDivs n) == n

{-
    5.2 Properties of Relations
    
    Reflexive       if for every 'x' in A: xRx
                    Ex. (<=) is reflexive is every number is
                             less than or equal to itself
                             
    Irreflexive     if for no 'x' in A is there a reflexive relation
    
    Symmetric       if for all x,y in A: if xRy then yRx
    
    Asymmetric      if for all x,y in A: if xRy not yRx
    
    Antisymmetric   if for all x,y in A: if xRy and yRx then x==y
                    an asymmetric relation is always antisymmetric
                    
    Transitive      if for all x,y,z in A: if xRy and yRz then xRz
    
    Intransitive    if for all x,y,z in A: if xRy and yRz then not xRz
    
    
    Quasi-order     if R on A is transitive and reflexive
     (pre-order)
     
    Partial order   if R on A is transitive, reflexive and antisymmetric
    
    Strict          if R on A is transitive and irreflexive
     Partial Order
        
    Linear Order    if for all x,y in A: xRy or yRx or x = y
                    (has the 'comparative property')
                    
    Total Order     a partial order that is also linear
    
    Example relations:
        
        successor   {(n,m) | n + 1 = m}
        divisor     {(n,m) | n divides m}
        coprime     nCm :equiv GCD(n,m) = 1
                    i.e. the only factor of n that divides m is 1
                         and the only factor of m that divides n is 1
                         
     
    
    Closures
    --------
        If O is the set of properties of relations on a set A,
        then the O-closure of a relation R is the smallest relation
        S that includes R and has all the properties in O.
        
        The most important closures are:
            reflexive, symmetric, transitive and reflexive
               
    Relational Composition
    ----------------------
        If R and S are relations on A then the composition of
        R and S is
            x RS z :equiv (exists y in A)(xRy && ySz)
            
        furthermore for n in N, n >= 1 we define R^n by means of
            R^1     := R
            R^(n+1) := R^n compose R
            
        (see Power of Relations in Discrete Math)
    
    Preservation of Properties
    --------------------------
    preserved under:       reflexivity  symmetry  transitivity
    -----------------------------------------------------------                             
        union                   yes       yes         yes
        intersection            yes       yes         no
        inverse                 yes       yes         yes
        complement              no        yes         no
        composition             yes       no          no
-}
{-
    5.2 Implementing Relations as Sets of Pairs
    
        The file SetOrd.hs contains a module for handling
        Sets as ordered lists without duplicates.
        
        The following functions rely on the module SetOrd

-}
type Rel a = Set (a,a)      -- type alias for a Relation

-- Returns the domain of the given relation as a Set
domR :: Ord a => Rel a -> Set a
domR (Set r) = list2set [x | (x,_) <- r]

-- Returns the range of the given relation as a Set
ranR :: Ord a => Rel a -> Set a
ranR (Set r) = list2set [ y | (_,y) <- r]

-- Creates the identity relation over the given set 
idR :: Ord a => Set a -> Rel a
idR (Set xs) = Set [(x,x) | x <- xs ]

-- Returns the total relation over the given set
totalR :: Set a -> Rel a
totalR (Set xs) = Set [(x,y) | x <- xs, y <- xs ]

-- Returns the inverse of the given relation (maps R to R^(-1))
invR :: Ord a => Rel a -> Rel a
invR (Set []) = (Set [])
invR (Set ((x,y):r)) = insertSet (y,x) (invR (Set r))

-- Check whether a pair is in a relation
inR :: Ord a => Rel a -> (a,a) -> Bool
inR r (x,y) = inSet (x,y) r

-- Returns the complement of a relation
complR :: Ord a => Set a -> Rel a -> Rel a
complR (Set xs) r = Set [(x,y) | x <- xs, y <- xs, not (inR r (x,y))]

-- Returns True if the given relation is reflexive on the given set
reflR :: Ord a => Set a -> Rel a -> Bool
reflR set r = subSet (idR set) r

-- Returns True if the given relation is irreflexive on the given set
irreflR :: Ord a => Set a -> Rel a -> Bool
irreflR (Set xs) r = all (\pair -> not (inR r pair)) [(x,x) | x <- xs]

-- Returns True if the given relation is symmetric
symR :: Ord a => Rel a -> Bool
symR (Set []) = True
symR (Set ((x,y):pairs)) 
    | x == y = symR (Set pairs)
    | otherwise = inSet (y,x) (Set pairs)
               && symR (deleteSet (y,x) (Set pairs))
               
-- Returns True if the given relation is transitive
transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s)  = and [trans pair (Set s) | pair <- s]
    where   trans (x,y) (Set r) =
                and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ]
                
-- Relation Composition operator
infixr 5 @@     -- right associative R composition operator

(@@) :: Eq a => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) = 
   Set (nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ])
   
-- Returns the result of composing a relation with itself 
-- (powers of a relation)
repeatR :: Ord a => Rel a -> Int -> Rel a
repeatR r n | n < 1     = error "argument < 1"
            | n == 1    = r
            | otherwise = r @@ (repeatR r (n-1))
            
-- Example of calculating powers of R
--  Note that R^3 is the same as the original relation, and,
--       that R^4 is the same as R^2
r  = Set [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
r2 = r @@ r             -- R^2 = {(0,0),(0,3),(1,2),(1,3),(2,2),(2,3)}
r3 = repeatR r 3        -- R^3 = {(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)}
r4 = repeatR r 4        -- R^4 = {(0,0),(0,3),(1,2),(1,3),(2,2),(2,3)}

ex51a = r == r3 && r2 == r4

-- had to modify SortOrd definitions to get this to work as expected
-- unionSet was returning an unsorted set with duplicates
s = Set [(0,0),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,2),(2,3)]
ex51b = (unionSet r (s @@ r)) == s
               
-- Exercise 52,53,54 - provided solutions
--               
-- Returns the intersection of two sets      
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) set2 = Set []
intersectSet (Set (x:xs)) set2
    | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
    | otherwise    = intersectSet (Set xs) set2
    
-- restrict a relation to a given set    
restrictR :: Ord a => Set a -> Rel a -> Rel a
restrictR set rel = intersectSet (totalR set) rel

-- Returns the reflexive closure of the given relation
rclosR :: Ord a => Rel a -> Rel a
rclosR r = unionSet r (idR background)
    where background = unionSet (domR r) (ranR r)
    
-- Returns the symmetric closure of the given relation
sclosR :: Ord a => Rel a -> Rel a
sclosR r = unionSet r (invR r)

-- Returns the transitive closure of the given relation
tclosR :: Ord a => Rel a -> Rel a
tclosR r | transR r  = r
         | otherwise = tclosR (unionSet r (r @@ r))

-- Exercise 55 - provided solutions
--      The 'inDegree' appears to be a count of number of times
--          'a' then occurs in the y position of the (x,y) pairs
--         
--      The 'outDegree' appears to be a count of the number of
--          times 'a' occurs in the x position of the (x,y) pairs
--
-- Returns the number of elements of A that are R-related to a
inDegree :: (Eq a) => Rel a -> a -> Int
inDegree (Set r) = \ x -> length [ y | (_,y) <- r, y == x ]

-- Returns the number of elements of A that a is R-related to
outDegree :: (Eq a) => Rel a -> a -> Int
outDegree (Set r) = \ x -> length [ y | (y,_) <- r, y == x ]    

-- Exercise 56 - provided solution
-- 'a' of A is a 'source' of R iff (indegree r a) == 0 and
-- (outDegree r a) >= 1
sources :: (Eq a) => Rel a -> Set a
sources (Set r) = Set [ x | x <- union (map fst r) (map snd r),
                            inDegree (Set r)  x == 0,
                            outDegree (Set r) x >= 1 ]

-- 'a' of A is a 'sink of R' iff (inDegree r a) >= 1 and
-- (outDegree r a) == 0                            
sinks :: (Eq a) => Rel a -> Set a
sinks (Set r) = Set [ x | x <- union (map fst r) (map snd r),
                          outDegree (Set r) x == 0,
                          inDegree (Set r)  x >= 1 ]
                          
{-
    5.4 Relations as Functions
    
    A 'characteristic' function is a function of type A -> {0,1}
    they 'characterize' subsets of a set A.
    
        f:A -> {0,1} 
        
    characterizes the set
    
        B = {a in A | f(a) = 1} is a subset of A
        
    The function 'divides' (shown below) is typical (characteristic)
    of a Haskell function; it takes two arguments and returns a
    result:
    
        f :: a -> b -> c
        
    Haskell provides a function, 'flip' that will reverse the order
    of the arguments. For example, 
    
            flip (<=) 3 4   -> (<=) 4 3  -> False
            flip (<=) 4 3   -> (<=) 3 4  -> True
        
    A characteristic function takes two arguments and returns a result; 
    however, sometimes we want to pass in arguments as a pair
        f:(a,a) -> Bool or (a,b) -> c.
        
    Haskell provides a function, 'uncurry' that allows us to 
    transform a function that of type a -> b -> c into a type
    (a,b) -> c; while 'curry' allows us to take a function of
    type (a,b) -> c into a function of type a -> b -> c
    
    Below are examples of curried and uncurried functions
    
-}                          
divides :: Integer -> Integer -> Bool
divides d n | d == 0    =  error "divides: zero divisor"
            | otherwise = (rem n d) == 0      

eq :: Eq a => (a,a) -> Bool
eq = uncurry (==)

lessEq :: Ord a => (a,a) -> Bool
lessEq = uncurry (<=)
       
-- it's a simple matter to define the inverse of a function
-- that takes a pair for an argument [to 'flip' the pair]
inverse :: ((a,b) -> c) -> ((b,a) -> c)
inverse f (x,y) = f (y,x)

--
--  The representation of relations as characteristic functions
--      

-- a type alias defining a relation as a function
type Rel' a = a -> a -> Bool

emptyR' :: Rel' a
emptyR' = \ _ _ -> False

list2rel' :: Eq a => [(a,a)] -> Rel' a
list2rel' xys = \ x y -> elem (x,y) xys

-- identity relation
idR' :: Eq a => [a] -> Rel' a
idR' xs = \ x y -> x == y && elem x xs

-- invert a relation
invR' :: Rel' a -> Rel' a
invR' = flip

-- check wither a pair is in a relation
inR' :: Rel' a -> (a,a) -> Bool
inR' = uncurry

-- check whether a relation is reflexive, irreflexive, symmetric
-- or transitive (on a domain given by a list)
reflR' :: [a] -> Rel' a -> Bool
reflR' xs r = and [r x x | x <- xs]

irreflR' :: [a] -> Rel' a -> Bool
irreflR' xs r = and [ not (r x x) | x <- xs]

symR' :: [a] -> Rel' a -> Bool
symR' xs r = and [ not (r x y && not (r y x)) | x <- xs, y <- xs ]

transR' :: [a] -> Rel' a -> Bool
transR' xs r = and [ not (r x y && r y z && not (r x z))
                    | x <- xs, y <- xs, z <- xs ]
                    
unionR' :: Rel' a -> Rel' a -> Rel' a
unionR' r s x y = r x y || s x y

intersR' :: Rel' a -> Rel' a -> Rel' a
intersR' r s x y = r x y && s x y

reflClosure' :: Eq a => Rel' a -> Rel' a
reflClosure' r = unionR' r (==)

symClosure' :: Rel' a -> Rel' a
symClosure' r = unionR' r (invR' r)

-- relation composition
compR' :: [a] -> Rel' a -> Rel' a -> Rel' a
compR' xs r s x y = or [ r x z && s z y | z <- xs ]

-- composition of a relation with itself
repeatR' :: [a] -> Rel' a -> Int -> Rel' a
repeatR' xs r n | n < 1 = error "argument < 1"
                | n == 1 = r
                | otherwise = compR' xs r (repeatR' xs r (n-1))
                
                
                    
         