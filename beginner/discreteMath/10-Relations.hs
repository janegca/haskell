-- Chapter 10 - Relations
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm

{-
    Binary Relations
    ----------------
        Used to describe the relation between two objects.
        
    Definition
    
    A binary relation R with type R :: AxB is a 'subset' of AxB,
    where A is the domain and B is the co-domain of R. For x in A and 
    y in B, the notation xRy means (x, y) in R (pairs are 'ordered')
    
    [NOTE: the domain consists of the Set of the first, x, elements in
           the ordered pairs; the codomain, of the 'y' elements in 
           the ordered pairs i.e. x is drawn from A but the domain
           is not always == to A, same for y and B otherwise you could
           not 'disjoint' domains/codomains (see Digraph section)]
    
    Examples:
     
    Given a set of people, P, {Bill, Sue, Pat} and a set of animals, A,
    {dog, cat} the relation 'has :: PxA' describes which person has
    which kind of animal. The relation can be expressed as:
    
        has = {(Bill,dog), (Sue,dog),(Sue,cat),(Pat,dog)}
        
    Let R be the set of real numbers. Then the (<)::RxR is the
    'less than' relation consisting of the set of ordered pairs
    (x,y). Because the set is infinite we cannot write it out in
    its entirety:
        (<) = { ..., (−35.2,−12.1), (−1, 2.7), ... }
        
    Relational databases are built on the relations between the
    stored data; often with n-nary versus purely binary relations
    (n-ary relations can be 3-ary, 4-ary, etc).
    
    Cross-Product - pair each element in one set with every element
                    in a second set
-}
{-          
    Digraph
    --------
    A visual representation of binary relations.
    
    Every element of the domain and co-domain in a digraph diagram is
    represented by a labelled dot (called an element or node) in the 
    graph, and every pair (x, y) in the relation is represented by an 
    arrow going from x to y (which may be called an arrow or arc).
    
    Graphs may contain nodes with no arrows.
    
    Some relations have same set as their domain and co-domain
        R :: A x A
    Sometimes the domain and co-domain are 'disjoint' (the domains
    have no elements in common)
    
    Definition 35
    
    Let A be a set and R a binary relation R::AxA. The diagraph
    D of R is the ordered pair D = (A,R).
    
    Example
    
    The digraph of relation R::AxA where R = {(1, 2), (2, 3)}
    and A = {1, 2, 3} is ({1, 2, 3}, {(1, 2), (2, 3)})
    
    An 'empty relation' has no 'arrows' (no ordered pairs).
    Two graphs of empty relations are only equivalent if they have
    the same domains and co-domains.
    
    Directed Path - a set arcs arranged in a sequence so that the
                    end point of one arc is the start of the next
                    arc
                    
    Example:
        {(1, 2), (2, 3), (3, 4)} and {(1, 3), (3, 1)} are both paths,
        but the set {(1, 2), (5, 6)} is not.
-}
{-
    Computing with Binary Relations
    -------------------------------
    A relation R::AxB with domain A and co-domain B can be 
    represented as a list of type [(A,B)].
    Each element in the list is of type (A,B) is a pair of the form 
    (x,y) where x::A is in the domain and y::B is in the co-domain.
    
    Two restrictions on the elements make the relation easier to
    compute with:
    1.  there is a finite set of elements in the relation
    2.  the types in the domain and co-domain must be in the Eq and
        Show classes
    
    We can use the following for the relation of colour complements

-}
data Colour = Red | Blue | Green | Orange | Yellow | Violet
    deriving (Eq, Show)
    
colourComplement :: Digraph Colour          
colourComplement =
    ([Red,Blue,Green,Orange,Yellow,Violet], -- domain
     [(Red,Green), (Green,Red),             -- co-domain
      (Blue,Orange), (Orange,Blue),
      (Yellow,Violet), (Violet,Yellow)])
    
{-
    To say 'the colour complement of red is green' we can write:
    
                Red colourComplement Green
            or  (Red,Green) in colourComplement
            
    Note that the 'co-domain' includes the ordered pairs
    (Red,Green) and (Green,Red); if either was omitted we'd have
    a different relation.
-}
{-
    Properties of Relations
    
    Reflexive 
    ---------
        Every element of the domain is related to itself.
        In a digraph, the node has a line back to itself.
                     
        Example
            R::AxA where A = {1,2,3} and R = {(1,1),(2,2),(3,3)}
            is reflexive. (see Exercise 2b)
            
        EQ, LE, GE are all reflexive relations on numbers
        NE, LT, GT are all non-reflexive relations of numbers
-}
a = [1,2,3]
digraph1 = (a,[(1,1), (1,2), (2,2), (2,3), (3,3)])
digraph2 = (a,[(1,2), (2,3), (3,1)])
digraph3 = (a,[(1,1), (1,2), (2,2), (2,3)])

ex32a = isReflexive digraph1    -- True
ex32b = isReflexive digraph2    -- False no pairs with equal elements
ex32c = isReflexive digraph3    -- False (missing (3,3))

{-
    Irreflexive
    -----------
        A relation is 'irreflexive' if no member of the domain
        is related to itself.
        
        An empty set is both reflexive and irreflexive but no
        non-empty set can have both properties.
        
        It can happen that a relation is neither reflexive nor
        irreflexive.
        
        Example:
            Given A = {1, 2, 3, 4, 5} and the relation 
            R = {(1, 3), (2, 4), (3, 3), (3, 5)}, we can see
            that R is NOT reflexive ( no (1,1) ) nor is it
            reflexive (contains (3,3) ).

        Cycle - a path that starts and stops at the same node
        
    Symmetric
    ---------
    
        If the order of a relation doesn't matter 
        i.e. if xRy it must always be true yRx
        then the relation is 'symmetric'.
        
        On a digraph, every arc from a to be will have
        a matching arc from b to a
        
        Examples:
            Sibling relations
            Equality on real numbers
            The relation R = {(1, 2), (2, 1), (2, 3), (3, 2)}
            
    Antisymmetric
    -------------
    
        An antisymmetric relation is one where for all distinct values 
        a and b, it is never the case that both aRb and bRa.
        
        Both of the following must be true for antisymmetric relations:
            (all x,y) in A. x /= y -> not(xRy && yRx)
            (all x,y) in A. x /= y -> not xRy || not yRx
            
        i.e. for two distinct values x and y, there is never more
             than one arrow connecting them
    
        Examples:
            The 'less than' (<) relationship as it can never be
            true that x < y and also y < x
            
            IsChildOf relationship
            
            {(1, 1), (1, 2), (2, 3), (3, 1)} is antisymmetric
            
            {(1, 2), (2, 1), (2, 3), (3, 1)} is not antisymmetric
                contains (1,2) and (2,1)
            
        The digraph of an antisymmetric relation may contain cycles
        For example, R = {(1, 2), (2, 3), (3, 1)} goes from 1 to 2 
        to 3 back to 1 and R = {(1,1)} has a trivial cycle; thus,
        the length of a cycle in an antisymmetric relation can be
        > 2 or equal to 1 BUT NEVER == 2.
            
    Transitive
    ----------
        A binary relation R::AxB is 'transitive' if
            (all x,y,z) in A. xRy /\ yRz -> xRz
            
        For example, 
            The equals (=) relation
            The AncestorOf relation
            
            if x < y and y < z then x < z
            
            R = {(1, 2), (2, 3), (1, 3)} is transitive as (1,3)
               is required by the presence of (1,2) and (2,3)
               
        A transitive relation adds a short cut for every path
        of length 2 or more. To make a relation transitive,
        we must continue adding pairs until the relation is
        transitive; this process is called the 'transitive closure'
        of a relation.
        
-}
{-
    10.5 Relational Composition
    
    We can think of a relation R::AxB as taking us from point x :: A
    to point y :: B assuming (x,y) in R.  And if there is another
    relation S::BxC and (y,z) in S with z::C then we can use R and
    then S to go from x to z.  This is called the 'composition'
    of R and S and is written as R;S.
    
    If R::AxB is a relation from set A to set B, and
    If R'::BxC is a relation from set B to set C, then
    R;R' consists of all pairs (a,c) such that there is an intermediate
    connecting point b. This means (a,b) in R and (b,c) in R'.
    
    Examples:
        If we have a relation 'Flight' that represents all flights
        between two different cities then 'Flight;Flight' represents all
        trips that can be made using exactly two connecting flights.
        
    When looking to create compositions, we start with the terminal
    nodes in one relation and look for them as starting nodes in 
    another relation.
    
    Example:
        Suppose we have R = {(1, 2), (2, 3), (3, 4)} and want to
        compose R;R.
        
        Find all ordered pairs (1,x).
            there is only one, (1,2)
            first application goes from 1 to 2 and the second (2,3)
            therefore, R;R should contain (1,3)
            
        Next find all ordered pairs (2,x)
            there is only one, (2,3)
            first application goes from 2 to 3 and the second (3,4)
            therefore R;R should contain (2,4)
            
        There is only one pair (3,4) and no (4,x) pairs so we cannot
        have any pairs of the form (3,x) in R;R.
        
        Our composition, R;R, is therefore {(1,2), (2,4)}
        
-}
{-
    10.6 Powers of Relation
    
    For a relation, R, the nth power is the composition R;R;...;R
    where R appears 'n' times. R^2 = R;R, R^1 = R, R^0 is the identity
    relation.
    
    When a relation is composed with itself, R^n, a path of length
    n in R from (a,b) causes there to be a single link (a,b) in R^n.
    
    Def: Let A be a set and R::AxA a relation defined over A.
         The nith power of R is defined as:
          
            R^0  = {(a,a) | a in A}
         R^(n+1) = R^n;R
         
    Example:
        Given R = {(2,3), (3,2), (3,3)}, compute R^4
        
        R^0             identity, contains a reflexive loop to every node
        R^1 = R^0;R = R by definition
        R^2 = R^1;R=R;R
            we need to take each pair (a,b) and see if there is a
            pair (b,c); if so, we need to put (a,c) into R^2
            Gives
                R^2 = {(2,2), (2,3), (3,2), (3,3)}
                
        R^3 = R^2;R
                compose {(2,2), (2,3), (3,2), (3,3)}
                with    {(2,3), (3,2), (3,3)}
                gives   {(2,2), (2,3), (3,2), (3,3)}
                
                so R^3 = R^2
            This means that any further power of R will also yield
            R^2 so R^4 = R^2
            
    Relational composition is associative so R^(a+b) = R^a;R^b
    
    Example of computing powers of a cyclic relation:
    
        R = {(a, b), (b, c), (c, a)}.
        
        R^1 = R
        R^2 = R;R
            = {(a,b), (b,c), (c,a)};{(a,b), (b,c), (c,a)}
            = {(a,c), (b,a), (c,b)}
            
        R^3 = R^2;R
            = {(a,c), (b,a), (c,b)};{(a,b), (b,c), (c,a)}
            = {(a,a), (b,b), (c,c)}
            
        R^4 = R^3;R
            = {(a,a), (b,b), (c,c)};{(a,b), (b,c), (c,a)}
            = {(a,b), (b,c), (c,a)}
            
            so R^4 equal R^1 and
            
        the pattern repeats: R^4=R^1, R^5=R^2, R^(n+3)=R^n
        
-}
{-
    10.7 Closure Properties of Relations
    
    In computing, DB relations are usually declared in two ways
    
    1. A basic relation, containing essential information, is specified
    2. A larger relation is derived from the basic one by adding the ordered pairs
       required to give it the special properties that are needed
       
    Example
        An airline keeps a set of all their direct flights.
        If a passenger want to know if they fly to a specific place
        the sales staff can check for derived (transitive) relations.
        
        A relation database derived in this way is called the 
        'closure' of the basic relation.
        
        "The closure of a relation R with respect to a given property
         is the smallest possible relation that contains R and has that
         property."
         
        "Closure is suitable for adding properties that require the 
         presence of certain ordered pairs."
         
         Example
            You can take the symmetric closure of a relation by 
            checking every (x,y) pair and adding a (y,x) pair if it
            is missing.
            
        Closure is not suitable for properties that require the 
        absence of certain pairs.
        
    Reflexive Closure  r(R)
        Contains all the arcs in the relation along with an arc
        from each node to itself. Denoted by r(R)
        
        Examples
            The reflexive closure of the relation {(1,2), (2,3)}
            over {1,2,3} is {(1,1), (2,2), (3,3), (1,2), (2,3)}
            
        To calculate a reflexive closure, add self-loops (x,x) to
        all the nodes x in the set A
        
    Symmetric Closure  s(R)
        Contains all the arcs in the relation along with the 
        converse of those arcs. i.e. if there is an arc (a,b)
        there should also be an arc (b,a). Denoted by s(R)
        
        The converse of a relation R::AxB is written as
            R^c = {(b,a) | (a,b) in R}
            
    Transitive Closure  
        The transitive closure of a relation is the union of
        all the powers of R.
        
        i.e. if A has 4 elements then the transitive closure
             of R::AxA would be:
             
                R^1 union R^2 union R^3 union R^4
                
                 
-}