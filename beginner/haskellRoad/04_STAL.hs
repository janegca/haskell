{-
    Chapter 4 - Sets, Types and Lists
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
import Data.List
import DB
import SetEq

{-
    The Comprehension Principle
    ---------------------------
        A 'set' is a collection into a whole of definite, distinct
        objects of our intuition or of our thought. The objects are
        called the elements (members) of the set.
                Georg Cantor (1845-1915), founding father of set theory
                
    Principle of Extensionality
    ---------------------------
        Sets that have the same elements are equal. (A set is
        completely determined by its elements).
        
    Subsets
    -------
        A set A is called a 'subset' of the set B, and B is a
        superset of A, if every member of A is also a member of B.
        
    Theorem 4.1
    -----------
        For all sets A, B, and C
        1. A is a subset of itself                  {reflexivity}
        2. If A is a subset of B and
           B is a subset of A then A = B            {antisymmetry}
        3. If A is a subset of B and
           B is a subset of C then A = C            {transitivity}
           
    Abstraction
    -----------
        If P(x) is a certain property of objects x, the
        abstraction:
                {x | P(x)}
        denotes the set of things x that have the property P
        and for every object 'a' in {x | P(x)} is equivalent
        with P(a)
        
        If 'f' is a function, then
                { f(x) | P(x) }
        denotes the set of things of the form f(x) where the object
        'x' has the property P
        
    Ordinary and Extraordinary Sets
    -------------------------------
        Ordinary sets don't have themselves as a member
        Extraordinary sets have themselves as a member
        Leads to Russell's Paradox:
        
            If R is a legitimate set then
                R in R <=> R not in R
                
        which is impossible.
        
    
    Halting Problem
    ---------------
        It is a well known fact that theres is no general test for
        checking whether a given procedure terminates for a
        particular input.
        
    Singleton      - a set with only one element
    Unordered Pair - a set of the form {a,b}
                     if 'a' = 'b' then {a,b} is a singleton
    Empty Set      - only one, {}, a set with no elements
                   - the Empty Set is a subset of EVERY set
                   
    Algebra of Sets
    ---------------
        Union A B           - all elements of A and B
        Intersection of A B - all elements in BOTH A and B
        Difference of A B   - All elements in A NOT IN B
        Disjoint            - A and B are 'disjoint' if their
                              intersection is an empty set
                              
                              
    Theorem 4.16 
    ------------
        For all sets A, B, and C
        
        Idempotence
            A intersect empty set   = Empty Set
            A union empty set       = A
            A intersectA            = A
            A union A               = A
            
        Commutativity
            A interset B = B intersect A
            A union B    = B union A
            
        Associativity
            A intersect (B intersect C) = (A interset B) intersect C
            A union (B union C) = (A union B) union C
            
        Distributivity
            A intersect (B union C) = (A intersect B) union
                                      (A intersect C)
            A union (B intersect C) = (A union B) interset
                                      (A union C)
    
    Complement
        The complement of set X and A, a subset of X, is X less
        all elements in A
        
    Symmetric Difference
        The set of all objects that are either in the set A or
        the set B BUT not in both
        
    Ordered Pair
        Element order matters
            (a,b) = (x,y) => a = x && b = y
            
    Products
        The Cartesian product of the sets A and B is the set
        of all pairs (a,b) where 'a' is in A and 'b' is in B
        eg {0,1} x {1,2,3} = {(0,1),(0,2),(0,3),(1,1),(1,2),(1,3)}
        
        
    Theorem 4.38
    ------------
        For arbitrary sets A and B, the following hold:
        
        1.(A x B) intersect (C x D) = (A x D) intersect (C x B)
        2.(A u B) x C = (A x C) u (B x C)
          (A i B) x C = (A x C) i (B x C)
        3.(A i B) x (C i D) = (A x C) i (B x D)
        4.(A u B) x (C u D) = (A x C) u (A x D) u (B x C) u (B x D)
        5.[(A - C) x B|u|A x (B-D)] subset (AxB) - (CxD)
        
-}
{-
    Lists
    -----
        Lists and Sets have different identity conditions.
        A list of [a,b] is different from a list [a,b,b]
        while the set {a,b} is the same as the set {a,b,b}
        
        Data type for Lists as defined in Haskell:
        
            data [a] = [] | a : [a] deriving (Eq, Ord)
            
        where,
            a       is a type variable
            Eq      if equality is defined for the objects of 
                    type 'a' then the relation carries over lists
                    of type 'a'
            Ord     if ordering is defined for the objects of
                    type 'a' then the order carries over to lists
                    of type 'a'
            
        Lists over type 'a' are either empty, [], or they consist
        of an element of type 'a' in front of (:) a list over
        type 'a' (:) is the 'concatenation' operator for lists
        
        Lists are ordered sets; they are the same if they are either
        (1) both empty
        (2) they start with the same element and their tails are the
            same
            
        Operations on Lists
        
        head        returns the first element in a list
        tail        returns everything but the head
        last        returns the last element in a list
        init        returns everything but the last element
        null        returns True if a list is empty
        numb        remove duplicate elements
-}
--
-- Exercise 46
--  Write your own definition of a Haskell opertion reverse that 
--  reverses a list
--
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

--
-- Exercise 47
--  Write a function 'splitList' that gives all the ways to
--  split a list of at least two elements in two non-empty parts
--
-- provided solution
splitList :: [a] -> [([a],[a])]
splitList [x,y]  = [([x],[y])]
splitList (x:zs) = ([x],zs): addLeft x (splitList zs)
    where addLeft u [] = []
          addLeft u ((vs,ws):rest) = (u:vs,ws): addLeft u rest

-- alternate solution using map        
split :: [a] -> [([a],[a])]
split [x,y]    = [([x],[y])]
split (x:zs) =
    ([x],zs) : (map (\ (us,vs) -> ((x:us),vs)) (split zs))
    
-- Database as list of lists --------------------------------------------
-- the database is defined in DB.hs which is imported at the top
-- of this file

-- extract various elements, removing duplicates
characters = nub [ x    | ["play",_,_,x]  <- db ]
movies     =     [ x    | ["release",x,_] <- db ]
actors     = nub [ x    | ["play",x,_,_]  <- db ]
directors  = nub [ x    | ["direct",x,_]  <- db ]
dates      = nub [ x    | ["release",_,x] <- db ]
universe   = nub (characters++actors++directors++movies++dates)

-- define lists of tuples for various elements
direct     = [ (x,y)   | ["direct",x,y]  <- db ]
act        = [ (x,y)   | ["play",x,y,_]  <- db ]
play       = [ (x,y,z) | ["play",x,y,z]  <- db ]
release    = [ (x,y)   | ["release",x,y] <- db ]

-- define predicate functions
charP       = \ x       -> elem x characters 
actorP      = \ x       -> elem x actors
movieP      = \ x       -> elem x movies 
directorP   = \ x       -> elem x directors
dateP       = \ x       -> elem x dates 
actP        = \ (x,y)   -> elem (x,y) act
releaseP    = \ (x,y)   -> elem (x,y) release 
directP     = \ (x,y)   -> elem (x,y) direct 
playP       = \ (x,y,z) -> elem (x,y,z) play
        
-- Conjunctive Queries
-- get all the actors who are also directors  
q1 = [x | x <- actors, directorP x ]     

-- get all actors that are also are directors, together with
-- the films in which they were acting
q2 = [ (x,y)  | (x,y) <- act, directorP x]
  
-- get all directors together with their films and their release
-- dates
q4 = [ (x,y,z) | (x,y) <- direct, (u,z) <- release, y == u]

-- give me all directors of films released in 1995, together with
-- those films
q5 = [ (x,y) | (x,y) <- direct, (u,"1995") <- release, y == u]

-- give all directors of films released after 1995 together with 
-- those films and their release dates
q6 = [ (x,y,z) | (x,y) <- direct, 
                 (u,z) <- release, y == u, z > "1995"]
                 
-- give the films in which Kevin Spacey acted
q7 = [ x | ("Kevin Spacey",x) <- act]
     
-- give me all films aftter 1997 in which William Hurt acted
q8 = [ x | (x,y) <- release, y > "1997", actP ("William Hurt", x)]
     
-- are there any films in which the director was also an actor?
q9 = q1 /= []

-- does the DB contain films directed by Woody Allen?
q10 = [ x | ("Woody Allen", x) <- direct ] /= []
q10' = directorP "Woody Allen"

--
-- Exercise 4.48
--  Translate the following into a query
--  "Get the films in which Robert De Niro or Kevin Spacey acted"
--
q48 = [ y | (x,y,_) <- play, x == "Robert De Niro" ||
                             x == "Kevin Spacey" ]
                             
--
-- Exercise 4.49
--  Translate the following into a query
--  "Give me all the films with Quentin Tarantino as an actor 
--   or director that appeared in 1994"                      
q49 = nub [ x | (x, "1994")             <- release,
                ("Quentin Tarantino",y) <- act,
                ("Quentin Tarantino",z) <- direct,
                x == y || x == z]
            
-- provided solution
q49' = nub ([ y | ("Quentin Tarantino",y) <- act, releaseP (y,"1994") ]
        ++  [ y | ("Quentin Tarantino",y) <- direct, releaseP (y,"1994") ])            
            
--
-- Exercise 4.50
--  Translate the following into a query
--  "Give me all films released after 1997 in which Wiliam Hurt 
--   did not act"
q50 = [ x | (x,y) <- release, y > "1997", 
          not (actP ("William Hurt", x))]           
          
{-
    Other List Operations useful when treating a list as a set
    (note that these all assume the list has no duplicates)
    
    delete      remove an element from the list
    elem        return True if the element is in the list
    notElem     return True if an element is not in the list
    union       return the union of two lists
    intersect   return the intersection of two lists
    (\\)        list difference operator
    
-}          
-- a function to add an element to a list
addElem :: a -> [[a]] -> [[a]]
addElem x = map (x:)

-- a function to generate sublists of a list
powerList :: [a] -> [[a]]
powerList []     = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

{- 
    The Empty List
    
    normally, Haskell  will not show an empty list unless it is
    clear, from the context, what the expected list type should be
    for example, 
        [x | x <- [1..10],x<0] will display []
        
    but the attempt to display [] or [[],[[]]] produce 'show' errors
    as there is no context given for the expected type
    
    To get around this, we can build our own type for an empty
    list

-}

-- giving a 'type' to an empty list
data S = Void deriving (Eq, Show)
empty :: [S]
empty = []

-- following will not produce errors, but display empty lists correctly
empty1 = powerList empty
empty2 = powerList (powerList empty)
empty3 = powerList (powerList (powerList empty))
empty4 = powerList (powerList (powerList (powerList empty)))

-- 
-- Exercise 4.53
--  Write functions genUnion and genIntersect for generalized list
--  union and generalized list intersection. The functions should
--  be of type [[a]] -> [a]. Note that genIntersect is undefined
--  on the empty list of lists.
--
-- provided solution
genUnion :: Eq a => [[a]] -> [a]
genUnion []       = []
genUnion [xs]     = xs
genUnion (xs:xss) = union xs  (genUnion xss)

-- provided solution
genIntersect :: Eq a => [[a]] -> [a]
genIntersect []       = error "list of lists should be non-empty"
genIntersect [xs]     = xs
genIntersect (xs:xss) = intersect xs (genIntersect xss)

{-
    A drawback of using Lists as Sets is in the way they define
    equality; two lists with identical members in a different
    order are NOT EQUAL while two sets with the same members, 
    regardless of order, ARE EQUAL.
    eg [1,2,3] /= [3,2,1]  {1,2,3} == {3,2,1}
    
    The 'SetEq' module defines a special data type for sets
    using lists to overcome this difficulty.
-}
--
-- Exercise 4.54
--  Give implementations of the operations unionSet, intersectSet
--  and differenceSet in terms of inSet, insertSet and deleteSet
--
-- provided solution
unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set [])     set2 = set2
unionSet (Set (x:xs)) set2 =
    insertSet x (unionSet (Set xs) (deleteSet x set2))

-- provided solution    
intersectSet :: (Eq a) => Set a -> Set a -> Set a
intersectSet (Set []) set2 = Set []
intersectSet (Set (x:xs)) set2
    | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
    | otherwise    = intersectSet (Set xs) set2    
    
-- provided solution    
differenceSet :: (Eq a) => Set a -> Set a -> Set a
differenceSet set1 (Set []) = set1
differenceSet set1 (Set (y:ys)) =
    differenceSet (deleteSet y set1) (Set ys)    
    
-- 
-- Exercise 4.55
--  In an implementation of sets as lists without duplicates, the
--  implementation of insertSet has to be changed. How?
-- provided solution: need to maintain the underlying list sort
insertSet' :: (Ord a) => a -> Set a -> Set a
insertSet' x (Set s) = Set (insertList x s)

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of
                            GT -> y : insertList x ys'
                            EQ -> ys
                            _ -> x : ys    
                            
--
-- Exercise 4.56
--  What would have to change in the module SetEq.hs to get a 
--  representation of the set as 0?
--                             