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
    
    Provided ans:
    
    No. If it did, then the end of each loop would have to have a 
    reflexive loop, because the relation is transitive. Thus it would 
    not be irreflexive
-}