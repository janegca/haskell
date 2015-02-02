-- Chapter 8 - Set Theory
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
{-
    Notation
    --------
        A set is a collection of objects called 'members' or 'elements'.
        A set must contain unique elements (no duplicates)
        A set can have any number of elements
        
        Sets are usually represented by upper case letters: A, B, C,...
        and the member of these sets by lower case letters: a, b, c, ...
        An empty set is represented by curly braces: {}
        
        A 'set comprehension' is a way of defining a set:
            { x | p x}
        where
            p x is an expression containing 'x' which is True or False
            
        and the whole thing means 'the set of 'x' such that 'p x' is
        true for every member'
        
        A more general rendering is:
        
                { f x | p x }
                
        which is read as "the results of applying 'f' to  all values of 
        'x' that satisfy 'p x'"; for example
        
                { sqrt x | x in {1,2,3,4} }
                
        is the set of the square roots of the numbers 1 through 4

-}
{-
    Basic Operations on Sets
    ------------------------
    
    Subset Relation
        A is a subset of B if all elements of A are also in B.
        A is a 'proper subset' of B if A is smaller than B
        i.e. if B contains elements that are not in A. 
        
    Equality
        Two sets are equal if they contain exactly the same elements.
        i.e. if they are subsets of each other
        
    Union
        The 'union' of A and B is the set that contains all elements
        of both A and B.
        
                { x | x in A OR x in B }
        
    Intersection
        The 'intersection' of A and B is the set that contains all
        elements that are in A and also in B i.e. the elements 
        that are in both sets A and B
        
                { x | x in A AND x in B }
        
    Difference
        The 'difference' of A and B is the set of all elements in A
        but NOT IN B.
        
                { x | x in A AND x NOT IN B }
                
    Disjoint
        Two sets A and B are 'disjoint' if they have NO elements 
        in common.
        
    Complement
        The 'complement' of a set A is 'everything' that is NOT IN A.
        To define the complement, we must first define the universe,
        U, of things we are interested in.
        
            Let U be the universe of discourse and A be a set. The
            complement of A, written A', is the set U - A.
            
        For example,
            If the Universe is the set of alphanumeric characters,
            then the complement of the set of digits is the set
            of  letters.
            
            If the universe is {1,2,3,4,5} then the complement of
            the set {1,2} is {3,4,5} [note that the complement
            of {1,2} would be written as {1,2}', just as the
            complement of A is written as A']
            
    Powerset
        The 'powerset' of A, written P(A), is the set of all subsets
        of A, including the empty set and itself.
        If A contains 'n' elements then P(A) contains 2^n elements.
        
        Examples:
            P({})      = { {} }
            P({a})     = { {}, {a} }
            P({a,b})   = { {}, {a}, {b}, {a,b} }
            P({a,b,c)} = { {}, {a}, {b}, {c}, {a,b}, {a,c}, {b,c}, {a,b,c}}
            
-}
{-
    Finite Sets with Equality
        A class of sets with importance to computing is a set with
        a limited number of elements and an equality function 
        that can be used to compare members of the set.
        
        Lists can be used to represent sets but care must be taken
        as:
        1. lists can have duplicate elements
        2. elements of a list are in a fixed order
        3. all elements of a list must have the same type
        
        In Haskell, we need to define our 'set list' as:
        
                (Eq, Show) => [a]
                
        This defines the type as one whose members can be compared
        (they belong to the Eq 'Equality' class) and whose members
        can be printed (shown).
        
        We can not define a set of functions as functions can not
        be equated (no way to tell if two functions are equal).
        
        The 'normal form' for lists representing sets is that they
        have no duplicate elements.
        
        While sets aren't ordered, lists are; in order to make
        comparison easier, it is best to define a set as an
        'ordered list' so we need to modify the set-list type as:
                (Eq, Ord, Show) => [a]
                
        There are three methods for defining set-lists:
            enumerated sets, sequences, and comprehensions
            
            enumerated set - just lists the items
            sequence       - items aren't fully enumerated
                             e.g. [1,2,..100]
                             
            comprehension  - list notation mimics set notation
                [ x^2   | x <- [0..n] ]
                [ x     | x <- [0..n], x `mod` 2 == 0]
                [ x + 3 | x <- [0..n], x < 10]

-}
{-
    Computing with Sets
        Following functions and operators can be used on 
        finite sets with equality. They all take lists with
        duplicated elements but return them as lists with
        unique elements. (They are defined in Stdm.lhs)

        type Set a = [a]
        
        normalForm :: (Eq a, Show a) => [a] -> Bool
        normalizeSet :: Eq a => Set a -> Set a
        
        (+++) :: (Eq a, Show a) => Set a -> Set a -> Set a  -- union
        (***) :: (Eq a, Show a) => Set a -> Set a -> Set a  -- intersect
        (~~~) :: (Eq a, Show a) => Set a -> Set a -> Set a  -- diff

        subset, properSubset ::
            (Eq a, Show a) => Set a -> Set a -> Bool        

        setEq :: (Eq a, Show a) => Set a -> Set a -> Bool      

        complement s = universe ~~~ s
-}
{-
    8.4 Set Laws
    
        Theorem 68
            If A is a subset of B and B is a subset of C
            then A is a subset of C.
            
        Theorem 69 - associative and commutative set laws
            For al sets A, B and C
            
            1. A union B                   = B union A
            2. A intersect B               = B intersect A
            3. A union (B union C)         = (A union B) union C
            4. A intersect (B intersect C) = (A intersect B) intersect C
            5. A difference B              = A intersect B'
            
        Theorem 70 and 71 - distributive laws
            The 'union' and 'intersect' operations distribute over
            each other.
            
            A intersect (B union C) = (A inter B) union (A inter C)
            A union (B intersect C) = (A union B) inter (A union C)
            
        Theorem 72 - De Morgan's Laws for sets
        
            Let A and B be arbitrary sets, then
            
                (A union B)'     = A' intersect B'
                (A intersect B)' = A' union B'
                         
-}
