-- Chapter 10 - Relations - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm

{-
    Exercise 1
    
    Work out the values of the following expressions, and then check
    your answer by evaluating the expressions with the computer.
-}
ex1a = domain [(1,100),(2,200),(3,300)]     -- [1,2,3]
ex1b = codomain [(1,100),(2,200),(3,300)]   -- [100,200,300]
ex1c = crossproduct [1,2,3] [4]             -- [(1,4),(2,4),(3,4)]

{-
    Exercise 2
    
    The following list comprehensions define a list of ordered pairs.
    What relations are represented by these lists? Give the domain and 
    the co-domain, as well as the pairs themselves.
    
    (a) [(a,b) | a <- [1,2],        -- domain
                 b <- [3,4]]        -- codomain
                                    -- crossproduct
                 
    (b) [ (a,b) | a <- [1,2,3],     -- domain
                  b <- [1,2,3],     -- codomain
                  a == b]           -- 'equality' relation
                  
    (c) [ (a,b) | a <- [1,2,3],     -- domain   [1,2]
                  b <- [1,2,3],     -- codomain [2,3]
                  a < b]            -- (<) relation
                  
    Note: while x and y are drawn from A and B, the 'domain'
          of a relation contains only the x elements of the
          ordered pairs and the codomain, the y elements.
          i.e. {(x,y)} is not always equal to {(A,B)}
          
-}

-- relation is a cross-product of AxB
ex2a  = [(a,b) | a <- [1,2], 
                 b <- [3,4]]            -- [(1,3),(1,4),(2,3),(2,4)
ex2aD = normalizeSet $ domain   ex2a
ex2aC = normalizeSet $ codomain ex2a

-- relation is a 'subset' of AxB
ex2b  = [(a,b) | a <- [1,2,3], 
                 b <- [1,2,3], a == b ]  -- [(1,1),(2,2),(3,3)]
ex2bD = normalizeSet $ domain   ex2b
ex2bC = normalizeSet $ codomain ex2b

-- a relation is a 'subset' of AxB
ex2c = [ (a,b) | a <- [1,2,3], 
                 b <- [1,2,3], a < b ]  -- [(1,2),(1,3),(2,3)]
ex2cD = normalizeSet $ domain   ex2c
ex2cC = normalizeSet $ codomain ex2c

{-
    Exercise 3
    
    For each of the following Digraph representations of a relation,
    draw a graph of the relation, work out whether it is reflexive and 
    whether it is irreflexive, and then check your conclusion using the 
    isReflexive and isIrreflexive functions:    
    
        ([1,2,3],[(1,2)])               -- irreflexive
        ([1,2],[(1,2),(2,2),(1,1)])     -- reflexive
        ([1,2],[(2,1)])                 -- irreflexive
        ([1,2,3],[(1,2),(1,1)])         -- neither

-}
dg3a = ([1,2,3],[(1,2)])
dg3b = ([1,2],[(1,2),(2,2),(1,1)])
dg3c = ([1,2],[(2,1)])
dg3d = ([1,2,3],[(1,2),(1,1)])

ex3a = (isReflexive dg3a, isIrreflexive dg3a)   -- (False,True)
ex3b = (isReflexive dg3b, isIrreflexive dg3b)   -- (True,False)
ex3c = (isReflexive dg3c, isIrreflexive dg3c)   -- (False,True)
ex3d = (isReflexive dg3d, isIrreflexive dg3d)   -- (False,False)

{-
    Exercise 4
    
    Determine whether each of the following relations on real numbers
    is reflexive and whether it is irreflexive. Justify your conclusions.
    
    (a) less than (<)
            irreflexive, a number cannot be less than itself and
                         equal to itself at the same time
                         
    (b) less than or equal to (<=)
            reflexive, a number can equal itself
            
    (c) greater than (>)
            irreflexive, a number can be equal to itself and 
            greater than itself at the same time
            
    (d) greater than or equal to (>=)
            reflexive, a number can always equal itself
            
    (e) equal (=)       
            reflexive, a number is always equal to itself
            
    (f) not equal (/=)
            irreflexive, a number is never not equal to itself
            so impossible to match on itself using /=

-}
{-
    Exercise 5
        Is the relation 'IsChildOf' symmetric?
        
        Ans: No. If A is a child of B then B cannot be a child of A
-}
{-
    Exercise 6
        Suppose we have a relation R :: A × A, where A is non-empty
    and reflexive, but it has only the arcs required in order to be 
    reflexive. Is R symmetric?
    
    Ans: Yes. If every element is only related to (equal to) itself,
         then the relation is symmetric.
-}

ex6 =  isSymmetric ([1,2,3], ex2b )     -- True

{-
    Exercise 7
        In the definition of a symmetric relation, can the variables x 
        and y can be instantiated by a single node?
        
        Ans: Yes as x and y have the same values and each node can
             point back to itself.
-}
{-
    Exercise 8
        First work out whether the relations in the following expressions
        are symmetric and whether they are antisymmetric, and then check 
        your conclusions by evaluating the expressions with Haskell:

        isSymmetric     ([1,2,3],[(1,2),(2,3)])       -- False
        isSymmetric     ([1,2],[(2,2),(1,1)])         -- True
        isAntisymmetric ([1,2,3],[(2,1),(1,2)])       -- False
        isAntisymmetric ([1,2,3],[(1,2),(2,3),(3,1)]) -- True
    
-}

ex8a = isSymmetric ([1,2,3],[(1,2),(2,3)])              -- False    
ex8b = isSymmetric ([1,2],[(2,2),(1,1)])                -- True
ex8c = isAntisymmetric ([1,2,3],[(2,1),(1,2)])          -- False
ex8d = isAntisymmetric ([1,2,3],[(1,2),(2,3),(3,1)])    -- True

{-
    Exercise 9
    
    Which of the following relations are symmetric? Antisymmetric?
    
    (a) The empty binary relation over a set with four nodes
            symmetric??
            
    (b) The  = relation     -- symmetric
    (c) The <= relation     -- symmetric
    (d) The <  relation     -- antisymmetric
-}
{-  
    Exercise 10
    
    Determine by hand whether the following relations are transitive,
    and then check your conclusion using the computer:
    
    isTransitive ([1,2],[(1,2),(2,1),(2,2)])    -- False, needs (1,2)
    isTransitive ([1,2,3],[(1,2)])              -- True

-}
ex10a =  isTransitive ([1,2],[(1,2),(2,1),(2,2)])
ex10b =  isTransitive ([1,2,3],[(1,2)])

{-
    Exercise 11
        Determine which of the following relations on real numbers are
        transitive: (=), (/=), (<), (<=), (>), (>=).
        
        x  = y && y  = z then x == z      transitive
        x /= y && y /= z then x /= z  -- not necessarily
        x  < y && y  < z then x <  z      transitive
        x <= y && y <= z then x <= z      transitive
        x >  y && y >  z then x >  z      transitive
        x >= y && y >= z then x >= z      transitive
-}
{-
    Exercise 12
    
    Which of the following relations are transitive?
    
    (a) The empty relation                  -- transitive
    (b) The IsSiblingOf relation            -- transitive
    (c) An irreflexive relation             -- transitive (?)   
    (d) The IsAncestorOf relation.          -- transitive

-}
ex12c1 = isIrreflexive dg3a && isTransitive dg3a 
ex12c2 = isIrreflexive dg3c && isTransitive dg3c

{-
    Exercise 13
    
    First work out by hand the ordered pairs in the following relational
    compositions, and then check your results using the computer:
    
    relationalComposition [(1,2),(2,3)] [(3,4)]
        there's a 3 to 4, no (4,x) but there is a (2,3)
        so we can have (2,4)
    
    relationalComposition [(1,2)] [(1,3)]
        1 to 2 with no (2,x)
        1 to 3 with no (3,x)
        
        empty composition
-}
ex13a = relationalComposition [(1,2),(2,3)] [(3,4)]     -- [(2,4)]
ex13b = relationalComposition [(1,2)] [(1,3)]           -- []

{-
    Exercise 14
    
     Find the composition of the following relations:
     
    (a) {(Alice, Bernard), (Carol, Daniel)} and {(Bernard, Carol)}.
         Bernard to Carol and from Alice to Bernard
         so need (Alice,Carol)
    
    (b) {(a, b), (aa, bb)} and {(b, c), (cc, bb)}
        (a,b)->(b,c) so need (a,c)
        
    
    (c) R;R, where the relation R is defined as
            R = {(1, 2), (2, 3), (3, 4), (4, 1)}.
            
        (1,x) (1,2) (2,3) (3,4) (4,1) so need
            (1,3) (2,4) (3,1) -- also (4,2)
        
    (d) {(1, 2)} and {(3, 4)}
            empty composition
    
    (e) The empty set and any other relation. -- an empty set
-}
ex14a = relationalComposition 
            [("Alice","Bernard"),("Carol","Daniel")]
            [("Bernard","Carol")]  -- [("Alice","Carol")]

ex14b = relationalComposition
            [("a","b"),("aa","bb")]
            [("b","c"),("cc","bb")]  -- [("a","c")]
            
ex14c = relationalComposition
            [(1,2),(2,3),(3,4),(4,1)]
            [(1,2),(2,3),(3,4),(4,1)] -- [(1,3),(2,4),(3,1),(4,2)
            
ex14d = relationalComposition [(1,2)] [(3,4)]       -- []

ex14e = relationalComposition ex14d [(1,2),(2,3)]   -- []

{-
    Exercise 15
        Work out the values of these expressions, and then evaluate
        them using the computer
        
        equalityRelation [1,2,3]        -- [(1,1),(2,2),(3,3)]
        equalityRelation ([]::[Int])    -- []
-}
ex15a = equalityRelation [1,2,3]        -- [(1,1),(2,2),(3,3)]
ex15b = equalityRelation ([]::[Int])    -- []

{-
    Exercise 16
    
    Calculate the following relational powers by hand, and then evaluate
    them using the computer.

    relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 1
    
        R^1 = R = [(1,2),(2,3),(3,4)]           
    
    relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 2
    
        R^2 = R^1;R
            = [(1,2),(2,3),(3,4)];[(1,2),(2,3),(3,4)]
            = [(1,3),(2,4)]
        
    relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 3
    
        R^3 = R^2;R
            = [(1,3),(2,4)]; [(1,2),(2,3),(3,4)]           
            = [(1,4)]
    
    relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 4   

        R^4 = R^3;R
            = [(1,4)];[(1,2),(2,3),(3,4)]           
            = []
-}
ex16a = relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 1
ex16b = relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 2
ex16c = relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 3
ex16d = relationalPower ([1,2,3,4],[(1,2),(2,3),(3,4)]) 4

{-
    Output:
    
        Main> ex16a
        [(1,2),(2,3),(3,4)]
        Main> ex16b
        [(1,3),(2,4)]
        Main> ex16c
        [(1,4)]
        Main> ex16d
        []
        Main>     
    
-}
{-
    Exercise 17
        Why do we not need to check the ordered pairs in R while
        calculating R^0;R ?
        
        Ans: R^0 is the identity function, it returns
            [(a,a),(b,b),...,(c,c)] and R is the original relation
            so R^0;R simply returns the original relation, R
-}
ex17r = [(1,2),(2,3),(3,4)]
ex17a = relationalPower ([1,2,3,4], ex17r) 0 -- [(1,1),(2,2),(3,3),(4,4)]
ex17b = relationalComposition ex17a ex17r    -- [(1,2),(2,3),(3,4)]

{-
    Exercise 18
        Why can we stop calculating powers after finding that two 
        successive powers are the same relation?
        
        Ans: Each iteration is the previous iteration composed with
             the original relation; once we have two successively
             identical computations we know combining with R will
             once again produce the same result
-}
{-
    Exercise 19
        What is R^4 where R is {(2, 2), (4, 4)}?
        
        Ans:
            R is its own equality relation so composing it with
            any power from R^0 to R^n will always result in itself.
-}
ex19a = [ (a,b) | a <- [2,4], b <- [2,4], a == b] -- [(2,2),(4,4)]
ex19b = relationalPower([2,4],[(2,2),(4,4)]) 0    -- [(2,2),(4,4)]
ex19c = relationalPower([2,4],[(2,2),(4,4)]) 4    -- [(2,2),(4,4)]

{-
    Exercise 20
    
        What is the relationship between adding new ordered pairs to
        make a relation transitive and taking the power of a relation?
        
        Ans:
            if you take enough nth powers, you end up with 'transitive'
            closure (all points will connected) if the relations
            can be closed.
-}
{-
    Exercise 21
        Suppose a set A contains n elements. How many possible relations
        with type R :: A x A are there?
        
        Ans: 2^(2^n)
-}
{-
    Exercise 22
        Given the relation {(a, b), (b, c), (c, d), (d, e)}, how many 
        times would we have to compose this relation with itself before 
        the empty relation is produced?
        
        Ans: since there is a direct path through a,b,c,d,e and no
             cycle, R^(length R) = R^5
-}

ex22a = relationalPower([1,2,3,4,5], [(1,2),(2,3),(3,4),(4,5)]) 4 --[(1,5)]
ex22b = relationalPower([1,2,3,4,5], [(1,2),(2,3),(3,4),(4,5)]) 5 --[]

{-
    Exercise 23
        Given the set A = {1, 2, 3} and the relation R :: A × A where
        R = {(3, 1), (1, 2), (2, 3)}, what is the value of R2? R3?
        
        Ans:
            R^2 = R^1;R
                = {(3,1), (1,2), (2,3)};{(3,1), (1,2), (2,3)}
                = {(3,2), (1,3), (2,1)
                
            R^3 = R^2;R
                = {(3,2), (1,3), (2,1); {(3,1), (1,2), (2,3)}
                = {(3,3), (1,1), (2,2)}

-}
ex23a = relationalPower([1,2,3], [(3,1),(1,2),(2,3)]) 2 
ex23b = relationalPower([1,2,3], [(3,1),(1,2),(2,3)]) 3 

{-
    Output:
    
        Main> ex23a
        [(1,3),(2,1),(3,2)]
        Main> ex23b
        [(1,1),(2,2),(3,3)]
        Main>     
-}

--
--  Closures
--
{-
    Exercise 24
    
        Work out the following reflexive closures by hand, and then
        check your results using the computer:

        reflexiveClosure ([1,2,3],[(1,2),(2,3)])
            [(1,1),(2,2),(3,3),(1,2),(2,3)]
        
        reflexiveClosure ([1,2],[(1,2),(2,1)])
            [(1,1),(2,2)(1,2),(2,1)]
-}

ex24a = reflexiveClosure ([1,2,3],[(1,2),(2,3)])
ex24b = reflexiveClosure ([1,2],[(1,2),(2,1)])

{-
    Output:
    
        Main> ex24a
        ([1,2,3],[(1,2),(2,3),(1,1),(2,2),(3,3)])
        Main> ex24b
        ([1,2],[(1,2),(2,1),(1,1),(2,2)])
        Main>     
-}
{-
    Exercise 25
    
        What is the reflexive closure of the relation R;R, where R is
        defined as {(1,2), (2,1)}?
        
        Ans: [(1,1), (1,2), (2,1), (2,2)]
-}
ex25 = reflexiveClosure ([1,2], [(1,2),(2,1)])

{-
    Exercise 26
        Work out the following symmetric closures by hand, and then
        calculate them using the computer window:

        symmetricClosure ([1,2],[(1,1),(1,2)])
            [(1,1),(1,2),(2,1)]
        
        symmetricClosure ([1,2,3],[(1,2),(2,3)])
            [ (1,2),(2,1),(2,3),(3,2)]
-}
ex26a = symmetricClosure ([1,2],[(1,1),(1,2)])
ex26b = symmetricClosure ([1,2,3],[(1,2),(2,3)]) 

{-
    Output:
    
        Main> ex26a
        ([1,2],[(1,2),(1,1),(2,1)])
        Main> ex26b
        ([1,2,3],[(1,2),(2,3),(2,1),(3,2)])
        Main>     
-}
{-
    Exercise 27
    
    What is the symmetric reflexive closure of the relation
    {(a, b), (b, c)}?
    
    Hint: take the reflexive closure first, followed by the symmetric 
    closure of the result.
    
    Ans:
        {(a,b),(b,c),(a,a),(b,b),(c,c),(b,a),(c,b)}

-}
ex27 = symmetricClosure (reflexiveClosure (
            ['a', 'b', 'c'], [('a', 'b'), ('b', 'c')]))
            
{-
    Output:
    
    Main> ex27
    ("abc",[('a','b'),('b','c'),
            ('b','a'),('c','b'),                -- symmetric
            ('a','a'),('b','b'),('c','c')])     -- reflexive
    Main>     
-}       
{-
    Exercise 28
        Find the reflexive symmetric closure of the relation {(a, c)}.
        
    Ans:    {(a,c), (c,a), (a,a), (c,c) }
-}     
ex28 = symmetricClosure(reflexiveClosure( "ac", [( 'a','c') ] ) )

{-
    Output:
    
        Main> ex28
        ("ac",[('a','c'),('c','a'),('a','a'),('c','c')])
        Main>     
-}
{-
    Exercise 29
    
    Work out the following transitive closures by hand, and then
    evaluate them using the computer:
    
    transitiveClosure ([1,2,3],[(1,2),(2,3)])
    
        R^1 = [(1,2),(2,3)]
        R^2 = [(1,2),(2,3)];[(1,2),(2,3)]
            = [(1,3)]
        R^3 = [(1,3)]; [(1,2),(2,3)]
        R^3 = []
        
        t(R) = [(1,2),(2,3),(1,3)]
    
    transitiveClosure ([1,2,3],[(1,2),(2,1)])
    
        R^1 = [(1,2),(2,1)]
        R^2 = [(1,2),(2,1)];[(1,2),(2,1)]
            = [(1,1), (2,2)]
        R^3 = [(1,1), (2,2)]; [(1,2),(2,1)]
            = [(1,2),(2,1)]
            
    cyclic, so transitive closure is [[(1,2),(2,1),(1,1),(2,2)]
-}
ex29a = transitiveClosure ([1,2,3],[(1,2),(2,3)])
ex29b = transitiveClosure ([1,2,3],[(1,2),(2,1)])

{-
    Output:
    
        Main> ex29a
        ([1,2,3],[(1,2),(2,3),(1,3)])
        Main> ex29b
        ([1,2,3],[(1,1),(2,2),(1,2),(2,1)])
        Main>     
-}
{-
    Exercise 30
    
    Given a digraph ({1, 2, 3, 4}, {(1, 2)}), what can we do to speed
    up the transitive closure algorithm, which requires that we take as 
    many powers of this relation as there are nodes in the digraph?
    
    Stop if an [] power relation or a fixed point relation is reached
-}
{-
    Exercise 31
    
    Find the transitive symmetric closure and the symmetric transitive
    closure of the following relations:
    
    (a) {(a, b), (a, c)}
    (b) {(a, b)}
    (c) {(1, 1), (1, 2), (1, 3), (2, 3)}
    (d) {(1, 2), (2, 1), (1, 3)}

-}
ex31a1 = transitiveClosure( symmetricClosure(
            "abc", [('a','b'),('a','c')] ))
            
ex31a2 = symmetricClosure( transitiveClosure(
            "abc", [('a','b'),('a','c')] ))
            
ex31b1 = transitiveClosure( symmetricClosure(
            "ab", [('a','b')]))
ex31b2 = symmetricClosure( transitiveClosure(
            "ab", [('a','b')]))
            
ex31c1 = transitiveClosure( symmetricClosure(
            [1,2,3], [(1,1), (1,2), (1,3), (2,3)]))
ex31c2 = symmetricClosure( transitiveClosure(
            [1,2,3], [(1,1), (1,2), (1,3), (2,3)]))
 
ex31d1 = transitiveClosure( symmetricClosure(
            [1,2,3], [(1,2), (2,1), (1,3)]))
            
ex31d2 = symmetricClosure( transitiveClosure(
            [1,2,3], [(1,2), (2,1), (1,3)]))

{-
    Output:
    
    NOTE: order matters, transitive symmetric closure is reflexive
                         symmetric  transitive closure is not
    
    Main> ex31a1
    ("abc",[('a','a'),('b','b'),('b','c'),('c','b'),('c','c'),
            ('a','b'),('a','c'),('b','a'),('c','a')])
    Main> ex31b1
    ("ab",[('a','b'),('b','a'),('a','a'),('b','b')])
    Main> ex31c1
    ([1,2,3],[(1,2),(1,3),(1,1),(2,2),(2,3),(2,1),(3,2),(3,3),(3,1)])
    Main> ex31d1
    ([1,2,3],[(2,3),(2,2),(1,1),(3,3),(3,2),(2,1),(1,3),(1,2),(3,1)])
    Main> 
    Main> ex31a2
    ("abc",[('a','b'),('a','c'),('b','a'),('c','a')])
    Main> ex31b2
    ("ab",[('a','b'),('b','a')])
    Main> ex31c2
    ([1,2,3],[(2,3),(1,2),(1,3),(3,2),(1,1),(2,1),(3,1)])
    Main> ex31d2
    ([1,2,3],[(2,3),(1,3),(1,1),(2,2),(3,2),(2,1),(3,1),(1,2)])
    Main>     

-}    
--
-- ORDER RELATIONS ------------------------------------------------------
--
{-
    Exercise 32
        Work out by hand whether the following digraphs are partial
        orders, and then check your results using the computer:
        
        isPartialOrder ([1,2,3], [(1,2),(2,3)])
        
        not reflexive, so not a partial order
        
        isPartialOrder ([1,2,3], [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)])
        
        reflexive (includes (1,1), (2,2) and (3,3)
        antisymmetric 
        transitive 1 to 2 to 3 therefore 1 to 3 which is included
-}
ex32a = isPartialOrder ([1,2,3], [(1,2),(2,3)])
ex32b = isPartialOrder ([1,2,3], [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)])

{-
    Output:
    
        Main> ex32a
        False
        Main> ex32b
        True
        Main>     
-}      
{-
    Exercise 33
    
    Calculate the following by hand, and then evaluate using the
    computer:
    
                    2 --- 3       2 precedes 3, so 3 is greatest
                    \     /
                       1          1 precedes 2 and 3 so weakest

    isWeakest [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)] 2  -- False
    isWeakest [(1,2),(1,3),(1,1),(2,2),(3,3)] 3        -- False
    
    isGreatest [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)] 3  -- True
    isGreatest [(1,2),(1,3),(1,1),(2,2),(3,3)] 1        -- False

-}
ex33a = isWeakest [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)] 2
ex33b = isWeakest [(1,2),(1,3),(1,1),(2,2),(3,3)] 3
ex33c = isGreatest [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)] 3
ex33d = isGreatest [(1,2),(1,3),(1,1),(2,2),(3,3)] 1    
      
{-
    Output:
    
        Main> ex33a
        False
        Main> ex33b
        False
        Main> :r
        Main> ex33c
        True
        Main> ex33d
        False
    
-}       
{-
    Exercise 34
    
    Calculate the following by hand, and then evaluate using the
    computer:
        weakestSet ([1,2,3,4],
                    [(1,4),(1,3),(1,2),(1,1),
                     (2,3),(2,4),(2,2),(3,4),
                     (3,3),(4,4)])
                     
        poset: 1 <- 2 <- 3 <- 4, so {1} is the weakest
                     
        weakestSet ([1,2,3,4],
                    [(1,4),(1,2),(1,1),(2,4),
                     (2,2),(3,4),(3,3),(4,4)])
                     
        poset: 1 <- 2 <- 4
                    3 <- 4  
               so weakest are {1,3}
        
        greatestSet ([1,2,3,4],
                     [(1,2),(3,4),(1,1),(2,2),(3,3),(4,4)])
                     
        poset: 1 <- 2
               3 <- 4      so greatest are {2,4}
        
        greatestSet ([1,2,3,4],
                     [(2,3),(3,4),(2,4),(1,1),(2,2),(3,3),(4,4)]) 
                     
        poset: 1; 2 <- 3 <- 4  so {4} is greatest
                               No, actually {1,4}, see output below
        
    (Solution Ref: https://gist.github.com/kanak/878086)

-}     
ex34a = weakestSet ([1,2,3,4],
                    [(1,4),(1,3),(1,2),(1,1),
                     (2,3),(2,4),(2,2),(3,4),
                     (3,3),(4,4)])
             
ex34b = weakestSet ([1,2,3,4],
                    [(1,4),(1,2),(1,1),(2,4),(2,2),(3,4),(3,3),(4,4)])

ex34c = greatestSet ([1,2,3,4],
                     [(1,2),(3,4),(1,1),(2,2),(3,3),(4,4)])

ex34d = greatestSet ([1,2,3,4],
                     [(2,3),(3,4),(2,4),(1,1),(2,2),(3,3),(4,4)])    
{-
    Output:
    
        Main> ex34a
        [1]
        Main> ex34b
        [1,3]
        Main> ex34c
        [2,4]
        Main> ex34d
        [1,4]
    
        NOTE: in ex34d '1' is not comparable with any other element
              as a result it is listed in both the greatest and 
              weakest sets
              
        Main>  weakestSet ([1,2,3,4], [(2,3),(3,4),(2,4),
                                       (1,1),(2,2),(3,3),(4,4)])
        [1,2]

-}
{-
    Exercise 35
    
    What are the greatest and weakest elements in a poset diagram
    that contains the following arcs:

    (a) {(a, b), (a, c)}            -- a weakest, b and c greatest
    (b) {(a, b), (c, d)}            -- a,c weakest, b,d greatest
    (c) {(a, b), (a, d), (b, c)}    -- a weakest, c,d greatest
    
    poset diagrams from https://gist.github.com/kanak/878086
    
    (a) c -> a <- b
    (b) a <- b; c <- d
    (c) d -> a <- b <- c
-}
{-
    Exercise 36
    
    Work out the following expressions, and evaluate them with the
    computer:

    isQuasiOrder ([1,2,3,4],[(1,2),(2,3),(3,4)])
        not reflexive
        1 <- 2
             2 <- 3 <- 4
             
        missing (1,3) so not transitive therefore False
        
    
    isQuasiOrder ([1,2,3,4],[(1,2)])
        1 <- 2 transitive, irreflexive therefore True

-}
ex36a = isQuasiOrder ([1,2,3,4],[(1,2),(2,3),(3,4)])    -- False
ex36b = isQuasiOrder ([1,2,3,4],[(1,2)])                -- True

{-
    Exercise 37
    
    Evaluate the following expressions, by hand and using the computer:

    isLinearOrder
    ([1,2,3],[(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)])

        1 < 2 < 3       True
    
    isLinearOrder
    ([1,2,3],[(1,2),(1,3),(1,1),(2,2),(3,3)])
    
        1 < 2; 1 < 3    False, missing (2,3)

-}
ex37a = isLinearOrder ([1,2,3],[(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)])
ex37b = isLinearOrder ([1,2,3],[(1,2),(1,3),(1,1),(2,2),(3,3)])

{-
    Output:
    
        Main> ex37a
        True
        Main> ex37b
        False
        Main>     

-}
{-
    Exercise 38
    
    We have been watching a computer terminal. Is the order in
    which people come and use the terminal a total order?
    
    We have the set of people using the machine; it can only
    be used by one person at a time, therefore it must, of
    necessity, be ordered by time.

-}
{-
    Exercise 39
    
    Is it always possible to count the elements of a linear order?
    
    No. The set of real numbers is linear but not countable as
    the number of elements in any set of reals is infinite.
    i.e. you cannot count the number of reals between 0 and 1.
    
-}
{-
    Exercise 40
    
    Can a set that is not a well order be countable?
    
    Yes. A nominal set can have no linear ordering but we can still
    process every element in the set. i.e. a handful of mustard seeds
    have no linear order but we can count them.
-}
{-
    Exercise 41
    
    Check to see that the following partial orders are not, in fact, total
    orders. Use the computer to generate a total order, using a topological
    sort.

    topsort ([1,2,3,4],[(1,2),(1,3),(2,3),(1,4),(2,4),
                        (1,1),(2,2),(3,3),(4,4)])
    
        poset:   1 <- 2 <- {3,4}
        topsort: 1 <- 2 <- 3 < 4

    topsort ([1,2,3],[(1,2),(1,3),(1,4),(1,1),(2,2),(3,3)])
    
        poset:  1 <- {2,3,4}
        topo sort: 1 <- 2 <- 3
-}
ex41a = topsort ([1,2,3,4],[(1,2),(1,3),(2,3),(1,4),(2,4),
                            (1,1),(2,2),(3,3),(4,4)])

ex41b = topsort ([1,2,3],[(1,2),(1,3),(1,4),(1,1),(2,2),(3,3)])

{-
    Output:
    
        Main> ex41a
        [1,2,3,4]
        Main> ex41b
        [1,2,3]
        Main>     

-}
{-
    Exercise 42
    
    Evaluate the following expressions using the computer:

-}
ex42a = isEquivalenceRelation ([1,2],[(1,1),(2,2),(1,2),(2,1)])
ex42b = isEquivalenceRelation ([1,2,3],[(1,1),(2,2)])
ex42c = isEquivalenceRelation ([1,2],[(1,1),(2,2),(1,2),(2,1)])
ex42d = isEquivalenceRelation ([1],[])    

{-
    Output:
    
        Main> ex42a
        True
        Main> ex42b
        False
        Main> ex42c
        True
        Main> ex42d
        False
        Main>     
-}
{-  
    Exercise 43
        Does the topological sort require that the graph’s relation is a
        partial order?
        
        A topological sort is the process of taking a partial order
        and putting it into total order. Guess you could take a
        total order and apply a topological sort but it would be
        trivial.
-}
{-
    Exercise 44
        Can the graph given to a topological sort have cycles?
        
        A topological sort puts elements into total order.
        Elements in total order create a 'chain'
        If the graph had cycles we'd get a repeated loop, not
        a chain; the sort would never end (?)
-}