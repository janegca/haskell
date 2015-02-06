-- Chapter 10 - Relations - Review Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm

{-
    Exercise 45
    
    Which of the following relations is an equivalence relation?
                                        Answers
                                        --------
    (a) InTheSameRoomAs                 yes
    (b) IsARelativeOf                   depends on how the relation
                                        is defined so no
    (c) IsBiggerThan                    no, not reflexive
    (d) The equality relation           yes
-}
{-
    Exercise 46
    
    Given a non-empty antisymmetric relation, does its transitive
    closure ever contain symmetric arcs?
    
    Ans:
    Yes. A transitive closure is the union of all power relations
    over a set, conceivably, one of these could contain symmetric
    arcs.
-}
ex46 = transitiveClosure ( [1,2,3] ,[(1,2),(2,3),(3,1)])

{-
    Output:
    
    Main> ex46
    ([1,2,3],[(1,2),(2,3),(3,1),(1,3),(2,1),(3,2),(1,1),(2,2),(3,3)])
    
-}    
    
{-
    Exercise 47
    
    What relation is both a quasi order and an equivalence relation?
    
    Ans: A quasi-order is irreflexive and transitive. An equivalence
         relation, is by definition, reflexive so the is no relation
         that is both.
         
    Provided Answer: The empty set.
-}
{-
    Exercise 48
    
    Write a function that takes a relation and returns True if that
    relation has a power that is the given relation
-}
-- NOTE on Original Answer:
--       flawed, using a list comparison, should be using setEq
--       see eqToPower' , written after seeing provided solution
--       and the solution given on https://gist.github.com/kanak/878086
eqToPower :: (Show a, Eq a) => Digraph a -> Bool
eqToPower dg = let r = snd dg
                   p = length r
               in and [relationalPower dg x == r | x <- [0..p]]

ex48a = eqToPower ([2,4],[(2,2),(4,4)])
ex48b = eqToPower ([1,2,3], [(3,1),(1,2),(2,3)])

-- alternative using higher order functions and setEq
eqToPower' :: (Show a, Eq a) => Digraph a -> Bool
eqToPower' dg@(s,r) = or $ map (setEq r . relationalPower dg) 
                               [2..length (domain r)]
                                                              
ex48e = eqToPower' ([2,4],[(2,2),(4,4)])
ex48f = eqToPower' ([1,2,3], [(3,1),(1,2),(2,3)])

-- provided solution
checkPowers :: (Eq a, Show a) => Digraph a -> Bool
checkPowers (set,relation)
    = any (setEq relation)
        [relationalPower (set,relation) n
         | n <- [2..length (domain relation)]]
               
ex48c = checkPowers ([2,4],[(2,2),(4,4)])
ex48d = checkPowers ([1,2,3], [(3,1),(1,2),(2,3)])

{-
    Exercise 49
    
    A quasi order is transitive and irreflexive. Can it have any
    symmetric loops in it?
    
    Provided answer:
    
    No. If it did, then the end of each loop would have to have a 
    reflexive loop, because the relation is transitive. Thus it would 
    not be irreflexive
-}
{-
    Exercise 50
    
    Given an antisymmetric irreflexive relation, could its transitive
    closure contain reflexive arcs?
    
    Provided answer: Yes. [(1,2),(2,3),(3,1)] is an example.
    
-}
{-
    Exercise 51
    
    Write a function that takes a relation and returns True if all of
    its powers have fewer arcs than it does.
-}
powerArcs :: (Show a, Eq a) => Digraph a -> Bool
powerArcs dg@(s,r) = and $ map ((< length r) . length . relationalPower dg) 
                               [2..length (domain r)]

ex51a = powerArcs ([1,2,3], [(3,1),(1,2),(2,3)])
ex51b = powerArcs ([2,4],[(2,2),(4,4)])

-- provided solution
fewerArcs :: (Eq a, Show a) => Digraph a -> Bool
fewerArcs (set,relation)
    = all (< (length relation)) [length (relationalPower (set,relation) n)
                                 | n <- [2..length (domain relation)]]
                                 
ex51c = fewerArcs ([1,2,3], [(3,1),(1,2),(2,3)])
ex51d = fewerArcs ([2,4],[(2,2),(4,4)])
                               
{-
    Exercise 52
    
    Write a function that takes a relation and returns True if the
    relation is smaller than its symmetric closure
-}
isRSmallerThanSc :: (Show a, Eq a) => Digraph a -> Bool
isRSmallerThanSc dg@(s,r) = (length r) < 
                            (length $ snd $ symmetricClosure dg)

ex52a = isRSmallerThanSc ([1,2],[(1,1),(1,2)])
ex52b = isRSmallerThanSc ([1,2,3,4,5], [(1,2),(2,3),(3,4),(4,5)])

-- provided solution
isSmaller :: (Ord a, Show a) => Digraph a -> Bool
isSmaller (set,relation)
    = let (symset,symrelation) = symmetricClosure (set,relation)
      in length relation < length symrelation

ex52c = isSmaller ([1,2],[(1,1),(1,2)])
ex52d = isSmaller ([1,2,3,4,5], [(1,2),(2,3),(3,4),(4,5)])

{-
    Exercise 53
    
    Given the partial order
        {(A,B), (B,C), (A,D)},

    which of the following is not a topological sort?
    
        [D,C,B,A]   
        [C,B,D,A]
        [D,C,A,B]   -- not a topological sort
        
    Ref:
        poset diagram: A <- B <- C; A <- D
-}
{-
    Exercise 54
        Is a reflexive and symmetric relation ever antisymmetric as well?
        
    Ans: Yes. 
         [(1,1),(2,2),(3,3)] is reflexive, symmetric and antisymmetric.
-}    
{-
    Exercise 55
    
    Given a relation containing only a single path of length n, how
    many arcs can be added by its symmetric transitive closure?
    
    Provided Answer:
        The number of arcs the transitive closure will add is
        1+2+...+n−1. The symmetric closure will double each of these, 
        so the total is 2(1+2+...+n−1).
        
-}
{-
    Exercise 56
    
    Given a relation containing only a cycle of length n containing
    all of the nodes in the domain, which power will be reflexive?
    
    Provided Ans: power n
-}
{-
    Exercise 57
    
    Can we write a function that determines whether the equality
    relation over the positive integers is reflexive?
        
    Provided ans: 
        No, because the function could not examine every arc 
        in the relation.
-}
{-
    Exercise 58
    
    Why can’t partial orders have cycles of length greater than 1?
    
    Provided ans:
        Given a partial order, assume that it has a cycle of length n. 
        Because it is transitive, it also has cycles of lengths
        1 to n - 1. But that means that it has a cycle of length 2,
        which cannot be because a partial order is antisymmetric.
        So it cannot both be a partial order and have cycles greater 
        than 1.
-}      
{-
    Exercise 59
    
    Is the last power of a relation always the empty set?
    
    Provided answer:
        No. Some have powers that repeat, such as the relation
        [(1,2),(2,1)]
-}      
{-
    Exercise 60
    
    The following list comprehension gives the arcs of a poset diagram.
    What kind of order relation does the diagram represent?
        [(a,a+1) | a <- [1..]]

    Ans: linear order  [(1,2),(2,3),(3,4),...]
-}
{-
    Exercise 61
    
    Is the composition of a relation containing only a single cycle
    with its converse the equality relation?
    
    Ans: Yes. (see provided example below)
-}  
data Colour = Red | Blue | Green | Yellow
    deriving (Show, Eq)
    
ex61 = relationalComposition 
            [(Red,Blue),(Blue,Green),(Green,Yellow),(Yellow,Red)]
            [(Blue,Red),(Green,Blue),(Yellow,Green),(Red,Yellow)]
            
{-
    Output:
    
        Main> ex61
        [(Red,Red),(Blue,Blue),(Green,Green),(Yellow,Yellow)]
        Main>     
-}            
{-
    Exercise 61
    
    Give examples of partial orders in which the set of greatest elements
    is the same as the set of weakest elements.
    
    Provided answer:
        The empty relation and the equality relation
-}      