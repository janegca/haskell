-- Chapter 8 - Set Theory - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
{-
    Summary of Set Laws

    Idempotent
        A union A     = A                         
        A intersect A = A
        
    Domination
        A union U      = U                        
        A intersect {} = {}
        
    Identity
        A union {}    = A
        A intersect U = A
        
    Double complement
        A = A''
        
    DeMorgan Laws
        (A union B)'     = A' intersect B'
        (A intersect B)' = A' union B'
        
    Commutative Laws
        A union B     = B union A
        A intersect B = B intersect A
        
    Associative Laws
        A union (B union C)         = (A union B) union C
        A intersect (B intersect C) = (A intersect B) intersect C
        
    Distributive Laws
        A intersect (B union C) = (A intersect B) union (A intersect C)
        A union (B intersect C) = (A union B) intersect (A union C)
       
    Absorption Laws
        A union (A intersect B) = A
        A intersect (A union B) = A
      
        
-}
import Stdm

{-
    Exercise 1
        Given the sets A = {1,2,3,4,5} and B = {2,4,6} calculate:
        
    (a) A union (B intersect A)
            B intersect A = {2,4}
            A union {2,4} = {1,2,3,4,5}
            
    (b) (A intersect B) union B
            A intersect B = {2,4}
            {2,4} union B = {2,4,6}
            
    (c) A - B
            A difference B = {1,3,5}
            
    (d) (B - A) intersect B
            B - A = {6}
            {6} intersect B = {6}
            
    (e) A union (B - A)
            B - A = {6}
            A union {6} = {1,2,3,4,5,6}

-}
{-
    Exercise 2
        Work out the value of the following and then check your
        results using the Hugs
        
-}
ex2a = (+++) [1,2,3] [3]                -- [1,2,3]
ex2b = (+++) [4,2] [2,4]                -- [2,4]
ex2c = (***) [1,2,3] [3]                -- [3]
ex2d = (***) [] [1,3,5]                 -- []
ex2e = (~~~) [1,2,3] [3]                -- [1,2]
ex2f = (~~~) [2,3] [1,2,3]              -- []
ex2g = (***) [1,2,3] [1,2]              -- [1,2]
ex2h = (+++) [1,2,3] [4,5,6]            -- [1,2,3,4,5,6]
ex2i = (***) ((~~~)[4,3] [5,4]) [1,2]   -- []
ex2j = (~~~)((+++)[3,2,4] [4,2]) [2,3]  -- [4]
ex2k = subset [3,4] [4,5,6]             -- False
ex2l = subset [1,3] [4,1,3,6]           -- True
ex2m = subset [] [1,2,3]                -- True
ex2n = setEq [1,2] [2,1]                -- True
ex2o = setEq [3,4,6] [2,3,5]            -- False
ex2p = (~~~) [1,2,3] [1]                -- [2,3]
ex2q = (~~~) [][1,2]                    -- []

{-
    Exercise 3
        The function
        
            powerset :: (Eq a, Show a) => Set a -> Set (Set a)
            
        takes a set and returns a powerset. Work out the values
        of the following expressions:
        
            powerset [3,2,4]
                [ [], [2], [3], [4], [2,3], [2,4], [3,4], [2,3,4] ]
                
            powerset [2]
                [ [], [2] ]
-}

ex3a = powerset [3,2,4]  -- [[3,2,4],[3,2],[3,4],[3],[2,4],[2],[4],[]]
ex3b = powerset [2]      -- [[2],[]]

{-
    Exercise 4
        The cross-product of two sets A and B is defined as:
            A x B = { (a,b) | a is in A, b is in B }
        
        The function
            crossproduct :: (Eq a, Show a, Eq b, Show b) =>
                    Set a -> Set b -> Set (a,b)
        
        takes two sets and returns their cross products.
        
        Evaluate:
            crossproduct [1,2,3] ['a','b']
                [ (1,'a'), (1,'b'), (2,'a'), (2,'b'), (3,'a'), (3,'b')]
                
            crossproduct [1] ['a','b']
                [ (1,'a'), (1,'b') ]
-}

ex4a = crossproduct [1,2,3] ['a','b']
ex4b = crossproduct [1] ['a','b']

{-
    output:
        Main> ex4a
        [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
        Main> ex4b
        [(1,'a'),(1,'b')]
        Main> 
-}    

{-
    Exercise 5    
        In the following exercise, let u be [1,2,3,4,5,6,7,8,9,10], a
        be [2,3,4], b be [5,6,7] and c be [1,2]. Give the elements of 
        each set:
-}

u,a,b,c :: Set Int
u = [1..10]
a = [2,3,4]
b = [5,6,7]
c = [1,2]

ex5a = (+++) a b                      -- [2,3,4,5,6,7]
ex5b = (***) ((~~~) u a) ((+++) b c)  -- [1,5,6,7]
ex5c = (~~~) c b                      -- [1,2]
ex5d = (+++) ((+++) a b) c            -- [1,2,3,4,5,6,7]
ex5e = (~~~) u a                      -- [1,5,6,7,8,9,10]
ex5f = (~~~) u ((***) b c)            -- [1..10]

{-
    Output:
    
        Main> ex5a
        [2,3,4,5,6,7]
        Main> ex5b
        [1,5,6,7]
        Main> ex5c
        [1,2]
        Main> ex5d
        [3,4,5,6,7,1,2]
        Main> ex5e
        [1,5,6,7,8,9,10]
        Main> ex5f
        [1,2,3,4,5,6,7,8,9,10]
        Main>     
-}
{-
    Exercise 6
        What are the elements of the set"
            { x + y | x in {1,2,3} /\ y in {4,5} }
-}

ex6 = [ x + y | x <- [1,2,3], y <- [4,5] ]  -- [5,6,6,7,7,8]

{-
    Exercise 7
        Write and evaluate a list comprehension that expresses the
        set:
            { x | x in {1,2,3,4,5} /\ x < 0 }
-}

ex7 = [ x | x <- [1..5], x < 0 ]        -- []

{-
    Exercise 8
        Write and evaluate a list comprehension that expresses
        the set:
            { x + y | x in {1,2,3} /\ y in {4,5} }
            
        Note: same as Exercise 6
-}

ex8 = [ x + y | x <- [1,2,3], y <- [4,5] ]

{-
    Exercise 9
        Write and evaluate the list comprehension that expresses
        the set:
        
            { x | x in {1..10} /\ even x }
-}

ex9 = [ x | x <- [1..10], even x ]      -- [2,4,6,8,10]

{-
    Exercise 10
        What is the value of each of the following expressions
-}

ex10a = subset [1,3,4] [4,3]    -- False
ex10b = subset []      [2,3,4]  -- True
ex10c = setEq  [2,3]   [4,5,6]  -- False
ex10d = setEq  [1,2]   [1,2,3]  -- False

{-
    Exercise 14
        The function
            smaller :: Ord a => a -> [a] -> Bool
            
        takes a value and a list of values and returns True if the 
        value is smaller than the first element in the list. Using this 
        function, write a function  that takes a set and returns its
        powerset. Use foldr    
       
    {Source of alternative methods:
        powerset' from
           /acmeism/RosettaCodeData/Task/Power-set/Haskell/power-set-3.hs
        powerList from 'The Haskell Road' Chapter 4
    }
    
    
-}
-- provided solution
smaller :: Ord a => a -> [a] -> Bool
smaller _ []    = True
smaller x (y:_) = x < y

powerSet :: (Ord a, Eq a) => [a] -> [[a]]
powerSet set = normalizeSet (foldr g [[]] set)
    where g x acc =
            [x:epset | epset <- acc,
                       not (elem x epset) 
                    && smaller x epset] ++ acc

-- alternative methods                    
powerset' :: [a] -> [[a]]
powerset' = foldr (\x acc -> (map (x:) acc) ++ acc) [[]]

powerList :: [a] -> [[a]]
powerList []     = [[]]
powerList (x:xs) = (map (x:) (powerList xs)) ++ (powerList xs)

{-
    Exercise 16
        Using a list comprehension, write a function that takes two sets
        and returns True if the first is a subset of the other.
-}

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset s1 s2 = and [ x == y | (x,y) <- zip s1 s2] 
                 && (length s1 <= length s2)
                 
-- provided solution
isSubset' :: Eq a => [a] -> [a] -> Bool
isSubset' set1 set2 = null [e | e <- set1, not (elem e set2)]

{-
    Exercise 17
        What is wrong with this definition of diff, a function that takes
        two sets and returns their difference?
        
        Returns the items in set2 not in set1, should be the other
        way around
-}
diff :: Eq a => [a] -> [a] -> [a]
diff set1 set2 = [e | e <- set2, not (elem e set1)]

diff' :: Eq a => [a] -> [a] -> [a]
diff' set1 set2 = [e | e <- set1, not (elem e set2)]
                 
{-
    Exercise 18
        What is wrong with this definition of intersection, a function
        that takes two sets and returns their intersection?
        
        Returns duplicated values; needs to be normalized.
        
        NOTE: the comprehension only returns elements if e == e
-}                 
intersection :: [a] -> [a] -> [a]
intersection set1 set2 = [e | e <- set1, e <- set2]

intersection' :: Eq a => [a] -> [a] -> [a]
intersection' set1 set2 = normalizeSet [e | e <- set1, e <- set2]

{-
    Exercise 19
        Write a function using a list comprehension that takes two sets
        and returns their union
        
        [Not sure you would use a list comprehension here??]
-}
union :: Eq a => [a] -> [a] -> [a]
union s1 s2 = normalizeSet (s1 ++ s2)

-- provided solution
union' :: Eq a => [a] -> [a] -> [a]
union' set1 set2 = set1 ++ [e | e <- set2, not (elem e set1)]