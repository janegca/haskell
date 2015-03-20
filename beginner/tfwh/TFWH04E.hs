module TFWH04E where

import Data.Char (toUpper, isAlpha)

{-
    Chapter 4 - List - Exercises
    
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/

-}
{-
    Notes:
    
    There are 3 kinds of lists
        finite  - built from (:) and []
                  eg 1:2:3:[]
                  
        partial - built from (:) and undefined
                  eg (<4) [1..] -> 1:2:3:undefined
                  
        infinite - built from (:) eg [1..]
        
    Notation for list enumeration:
    
        [m..n]      for [m, m+1, ..., n]
        [m..]       for the infinite list [m, m+1, m+2, ...]
        [m,n..p]    for the list [m, m+1(n-m), m+2(n-m),...,p]
        [m,n..]     for the infinite list [m, m+1(n-m),m+2(n-m)..]
    
-}
-- list comprehension examples
ex1 = [ x * x | x <- [1..5] ]                    -- [1,4,9,16,25]
ex2 = [ x * x | x <- [1..5], even x]             -- [4,16]

ex3 = [(i,j) | i <- [1..5], even i, j <- [i..5]]
        -- [(2,2),(2,3),(2,4),(2,5),(4,4),(4,5)]
        
ex4 = [x | xs <- [[( 3,4)],[( 5,4),( 3,2)]], (3, x) <- xs] -- [4,2]

-- Pythagorean Triads: x^2 + y^2 = z^2
triads :: Int -> [(Int,Int,Int)]
triads n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n],
                      x*x + y*y == z*z ]
                      
ex5 = triads 15
                
-- a more efficient approach, includes only unique triads and no
-- multiples of basic triads
triads' :: Int -> [(Int,Int,Int)]
triads' n = [(x,y,z) | x <- [1..m], y <- [x+1..n],
                      coprime x y,
                      z <- [y+1..n], x*x+y*y==z*z]
    where 
        m = floor (fromIntegral n / sqrt 2)

        divisors x  = [d | d <- [2..x-1], x `mod` d == 0]
        coprime x y = disjoint (divisors x) (divisors y)

        disjoint :: [Int] -> [Int] -> Bool
        disjoint xs [] = True
        disjoint [] ys = True
        disjoint (x:xs) (y:ys)
            | x < y  = disjoint xs (y:ys)
            | x == y = False
            | x > y  = disjoint (x:xs) ys                
          
ex5a = triads' 15
          
{-
    Functor Laws of map
    -------------------
        The name 'functor' comes from Category Theory.
    
        map id      = id
            applying the identity function to each element of a list
            leaves the list unchanged
            
        map (f . g) = map f . map g
            applying g to every element of a list followed by 
            applying f to every element of a list is the same as
            applying the composition (f . g) to every element of
            a list with the added bonus that only ONE list traversal
            is needed
        
    Haskell has a class, Functor, that declares the function 'fmap'
    which abstracts the above notions so they can be applied over
    any data structure (not just lists). In fact, map is just a
    synonym for 'fmap' over lists. (see Tree example below)
    
    'Natural transformation' or 'naturality' laws involving map:
        
        f . head       = head . map f
        map f . tail   = tail . map f
        map f . concat = concat . map (map f)
        
    Essentially, you can apply the operation to a list and then 
    change each element or you can change each element and then
    apply the operation; the results will be the SAME
        
-}          
data Tree a = Tip a | Fork (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Tip x)    = Tip (f x)
    fmap f (Fork u v) = Fork (fmap f u) (fmap f v)
    
ex7a = (map even . reverse) [1..20] == (reverse . map even) [1..20]
ex7b = (concat . map concat) [["a"],["b","c"],["d","e","f"]]
    == (concat . concat)     [["a"],["b","c"],["d","e","f"]]
    
{-
    A property of filter
    --------------------
    
        filter p . map f = map f . filter (p . f)

-}    

ex7c = (filter even . map (*3)) [1..20]
    == (map (*3) . filter (even . (*3))) [1..20]
    
-- example of using zipWith to build a list of pairs from two lists
ex7d = zipWith (,) ['a'..'z'] [1..]
    
-- example of using zipWith to check if list values are non-descending 
nondec :: Ord a => [a] -> Bool
nondec xs = and (zipWith (<=) xs (tail xs))

ex7e = nondec [10,9..0]
ex7f = nondec [1..10]
    
-- find the first position of a given value in a given list
-- or (-1) if it is not in the list
position :: Eq a => a -> [a] -> Int
position x xs = head ( [j | (j,y) <- zip [0..] xs, y == x] ++ [-1])

ex7g = position 'm' ['a'..'z']

-- a definition of sort using a 'divide and conquer' approach
sort :: Ord a => [a] -> [a]
sort []  = []
sort [x] = [x]
sort xs  = merge (sort ys) (sort zs)
    where
        (ys, zs) = halve xs
        
        halve xs = (take n xs, drop n xs)
        n        = div (length xs) 2
            
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs'@(x:xs) ys'@(y:ys) | x <= y = x : merge xs ys'
                            | otherwise = y : merge xs' ys

ex8a = sort "lmabdzgh"
ex8b = merge "lmabdzgh" "pqrnx"     -- result is unsorted
                          
-- ---------------------------------------------------------------------
-- Exercises
-- ---------------------------------------------------------------------
                          
-- Exercise B
allPairs = [ (x,y) | x <- [0..5], y <- [1..5]]

-- provided solution
-- lists all pairs in order of their sums
allPairs' = [ (x, d-x) | d <- [0..], x <- [0..d] ]

{- Exercise D

    [ e | x <- xs, p x, y <- ys]
    [ e | x <- xs, y <- ys, p x]
    
    Both will always produce the same result but the first version
    will be less expensive, only producing 'y' when 'p x' is True
    
    -- provided Answer
    They deliver the same result only if ys is infinite
    When that is so, the first method is the more efficient

-}  

-- Exercise E
-- write a function to list the different quadruples (a,b,c,d) where
-- 0 <= a, b, c, d <= n such that a^3 + b^3 == c^3 + d^3
-- provided solution (takes quite awhile to run)
--      a < b and c > a
quads :: Int -> [(Int,Int,Int,Int)]
quads n = [(a,b,c,d) | a <- [1..n],  b <- [a..n],
                       c <- [a+1..n],d <- [c..n],
                       a^3 + b^3 == c^3 + d^3]                        

exEa = quads 4200   -- first two numbers are 1,729 and 4,104                        
                        
{- Exercise F

    A dual view of construction lists is to construct them
    by adding elements to the end of the list
    
        data List a = Nil | Snoc (List a) a
        
    where 'Snoc' is the reverse of 'Cons'.
    The list [1,2,3] would be constructed as:
    
        Snoc (Snoc (Snoc Nil 1) 2) 3)
        
    Give the definitions for 'head' and 'last', and define two
    functions for converting efficiently from one view to
    the other.
    
        toList :: [a] -> List a
        fromList :: List a -> [a]
-}                        
exLst = Snoc (Snoc (Snoc Nil 1) 2) 3

data List a = Nil | Snoc (List a) a deriving Show

head' :: List a -> a
head' (Snoc Nil x) = x
head' (Snoc xs _)  = head' xs

exFa = head' exLst      -- 1

tail' :: List a -> List a
tail' (Snoc Nil x) = Nil
tail' (Snoc xs x)  = Snoc (tail' xs) x

exFb = tail' exLst      -- Snoc (Snoc Nil 2) 3

-- OOPs, was supposed to do 'last', not tail
last' :: List a -> a
last' (Snoc xs x) = x

exFb1 = last' exLst     -- 3

toList :: [a] -> List a
toList lst = convert (reverse lst)  -- again, THINK FUNC COMP
    where 
        convert [x]    = Snoc Nil x
        convert (x:xs) = Snoc (convert xs) x
                   
exFc = toList [1,2,3]         -- Snoc (Snoc (Snoc Nil 1) 2) 3       

-- provided solution
toList' :: [a] -> List a
toList' = convert . reverse
    where
        convert []     = Nil
        convert (x:xs) = Snoc (convert xs) x
        
exFc1 = toList' [1,2,3]         -- Snoc (Snoc (Snoc Nil 1) 2) 3       

fromList :: List a -> [a]
fromList lst = reverse (convert lst)
    where
        convert (Snoc Nil x) = [x]
        convert (Snoc xs x)  = x : convert xs
        
exFd = fromList exLst           -- [1,2,3]

-- provided solution
fromList' :: List a -> [a]
fromList' = reverse . convert
    where
        convert Nil = []
        convert (Snoc xs x) = x : convert xs
        
exFd1 = fromList exLst        -- [1,2,3]

{- Exercise H

    Give recursive definitions for the Prelude functions 'take' and
    'drop'.
    
    What are the values of
        take 0 undefined
        take undefined []
        
    according to your definitions?
        take' 0 undefined  = [] if undefined is an infinite list
        take' undefined [] = []
     
    Which of the following are valid for all integers m and n?
    
        take n xs ++ drop n xs = xs               -- valid
        take m . drop n = drop n . take (m+n)     -- valid
        take m . take n = take (min m n)          -- valid
        drop m . drop n = drop (m+n)              -- valid
        
    The standard prelude definiton for splitAt is:
        
        splitAt n xs = (take n xs, drop n xs)
        
    Give a definition of splitAt that traverse the list only once.
-}     
take' :: Int -> [a] -> [a]
take' n (x:xs) | n == 0    = []
               | otherwise = x : take' (n-1) xs            
take' _ [] = []

-- provided solution
take'', drop'' :: Int -> [a] -> [a]
take'' n [] = []
take'' n (x:xs) = if n == 0 then [] else x : take'' (n-1) xs

drop'' n [] = []
drop'' n (x:xs) = if n == 0 then x:xs else drop'' (n-1) xs

-- these definitions give
--      take'' undefined [] = []
--      take'' 0 undefined  = undefined

-- a third alternative to take that gives
--      take undefined [] = []
--      take 0 undefined  = []
-- (basically, the same as my orig take
takeA :: Int -> [a] -> [a]
takeA n xs | n == 0    = []
           | null xs   = []
           | otherwise = head xs : take (n-1) (tail xs)

exGa = take' 5  [1..]  == take 5  [1..]       -- True 
exGb = take' 20 [1..5] == take 20 [1..5]      -- True

drop' :: Int -> [a] -> [a]
drop' n (x:xs) | n == 1    = xs
               | otherwise = drop' (n-1) xs     
drop' _ [] = []

exGc = drop' 5 [1..10] == drop 5 [1..10]    -- True
exGd = drop' 5 [1..3]  == drop 5 [1..3]     -- True

exGe = take 5 [1..20] ++ drop 5 [1..20] == [1..20]              -- True
exGf = (take 3 . drop 5) [1..20] == (drop 5 . take 8) [1..20]   -- True
exGg = (take 3 . take 5) [1..20] == take (min 3 5) [1..20]      -- True
exGh = (drop 3 . drop 5) [1..20] == drop 8 [1..20]              -- True

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n lst = split n [] lst
    where
        split n xs (y:ys) | n == 0    = (reverse xs, y:ys)
                          | otherwise = split (n-1) (y:xs) ys
        split _ _ [] = ([],[])
   
exGi = splitAt' 5 [1..10] == splitAt 5 [1..10]  -- True

-- provided solution
splitAt'' :: Int -> [a] -> ([a],[a])
splitAt'' n [] = ([],[])
splitAt'' n (x:xs) = if n==0 then ([],x:xs) else (x:ys, zs)
    where (ys,zs) = splitAt'' (n-1) xs
    
exGi1 = splitAt'' 5 [1..10] == splitAt 5 [1..10]    -- True
   
{- Exercise I
    Which of the following statements do you agree with respec
    to the following equation:
    
        map (f . g) xs = map f (map g xs)
        
 x  1. It's not true for all xs; depends on whether xs is finite or not.
    4. It looks true but has to be proved from the definition of map
       and function composition
    5. Used right-to-left it looks like a program optimization; two
       list traversals are replaced by one.
       
    Correct Answers were 3, 4, 5 and 7
    
    3. It’s true for all lists xs, finite, partial or infinite, and for
       all f and g of the appropriate type. 
       In fact map (f . g) = map f . map g is a much neater alternative.
       
    7. Whether or not it is computed in pieces or as a whole, the 
       right-hand side does produce an intermediate list, while the 
       left-hand side doesn’t. It is a rule for optimising a program 
       even under lazy evaluation.
-}
{-  Exercise J

    Which of the following are True, which False:
    
    a.  map f . take n  = take n  . map f                     -- True
    b.  map f . reverse = reverse . map f                     -- True
    c.  map f . sort    = sort    . map f                     -- False
    d.  map f . filter p = map fst . filter snd . map (fork (f,p)) -- True
    e.  filter (p . g) = map (invertg) . filter p . map g   -- False (No!)
    f.  reverse . concat = concat . reverse . map reverse   -- True
    g.  filter p . concat = concat . map (filter p)         -- False (No!)
        
    where invertg satisfies invertg . g = id
    and fork is defined as shown below
        
-}
fork :: (a -> b, a -> c) -> a -> (b,c)
fork (f,g) x = (f x, g x)

exJa = (map (*2) . take 5) [1..10] == (take 5 . map (*2)) [1..10]     -- T
exJb = (map even . reverse) [1..10] == (reverse . map even) [1..10]   -- T
exJc = (map even . sort) [10,9..1] == (sort . map even) [10,9..1]     -- F
exJd = (map (*3) . filter even) [1..10]
    == (map fst . filter snd . map (fork ((*3),even))) [1..10]        -- T
exJe = undefined
exJf = (reverse . concat) ["Haskell","Curry"]
    == (concat . reverse . map reverse) ["Haskell","Curry"]           -- T
exJg = (filter (>4) . concat) [[1..5],[3..10],[1..3]]
    == (concat . map (filter (>4))) [[1..5],[3..10],[1..3]]           -- T
  
{-  Exercise K
    
    Define unzip and cross by:
    
        unzip = fork (map fst, map snd)
        cross (f, g) = fork (f . fst, g . snd)
        
    What are the types of these functions?
    
    Prove by equational reasoning that:
    
        cross (map f, map g) . unzip = unzip . map (cross (f,g))
        
    Using the functor laws of map and the following rules:
    
        1. cross (f,g) . fork (h,k) = fork (f . h, g . k)
        2. fork (f,g) . h = fork (f . h, g . h)
        3. fst . cross (f,g) = f . fst
        4. snd . cross (f,g) = g . snd
        
    Ans:  WRONG!! - SEE PROV SOLUTION
        cross (map f, map g) . unzip
        {functor law}
     -> unzip . cross (map f, map g)
        {def. of cross}
     -> unzip . fork ((map f) . fst, (map g) . snd)
        {function comp}
     -> unzip . fork (map (f . fst), map (g . snd))
        {function comp}
     -> unzip . map (fork (f . fst, g .snd))
        {def. of cross}
     -> unzip . map (cross (f,g))
     
    Provided Answer (from right to left):
        unzip . map (cross (f, g))
        {def. of unzip
     -> fork (map fst, map snd) . map (cross (f,g))
        {law of fork}
     -> fork (map fst . map (cross (f,g)), map snd . map (cross(f,g)))
        {law of map}
     -> fork (map (fst . cross(f,g)), map (snd . cross(f,g)))
        {laws fst and snd}
     -> fork (map (f . fst), map (g .snd))
     
    Note that the proof has been carried out at the function level
    in what is called 'point-free' style. Such a style often
    requires the use of 'plumbing combinators' such as 'fork' and
    'cross' to pass arguments to functions. 
    
    'Plumbing combinators' push values around, duplicate them and
    even elimnate them. Others, in the Prelude, are:
        const, curry and uncurry
    
        
-}  
unzip' :: [(a,a)] -> ([a],[a])
unzip' = fork (map fst, map snd)

exKa = unzip' [(1,2),(3,4),(5,6)]       -- ([1,3,5],[2,4,6])

cross :: (a -> c, b -> d) -> (a,b) -> (c,d)
cross (f,g) = fork (f . fst, g . snd)

{-  Exercise L

    Continuing from Exercise I, prove that
    
        cross (f,g) . cross (h,k) = cross (f . h, g . k)
        
    Ans: -- NOPE, SEE PROVIDED SOLUTION
            cross (f,g) . cross (h,k)
            {def. of cross}
         -> fork (f . fst, g . snd) . fork (h . fst, k . snd)
            {function comp}
         -> fork ( (f .fst) . (h . fst), (g .snd) . (k . snd) )
            {function comp}
         -> fork ( (f . h) . fst, (g . k) . snd )
            {def. of cross}
         -> cross ( f . h, g . k )
         
    Provided Solution:
            cross (f,g) . cross (h,k)
            {def. cross}
         -> cross (f,g) . fork (h . fst, k .snd)
            {law of cross and fork}
         -> fork (f . h . fst, g . k . snd)
            {def. of cross}
         -> cross (f . h, g . k)
         
         
    We also have cross (id,id) = id so cross has functor like
    properties and is, in fact, a bifunctor; which suggests the
    following generalization:
    
        class Bifunctor p where
            bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
            
    the arguments of 'bimap' are given separately rather than
    as a pair. Express cross in terms of bimap for the Pair
    of Bifunctor where
        type Pair a b = (a,b)
        
    Provided Solution:
        cross = uncurry bimap
        
    Either is a bifunctor.
    
-}