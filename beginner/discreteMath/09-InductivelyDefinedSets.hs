-- Chapter 9 - Inductively Defined Sets
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
import Stdm

{-
    There are different ways to define a set:
    
    enumeration     - simplest, just name the elements
                    - ok for small, finite sets
                    - impractical for large sets
                    
    ellipses        - use of ellipses to indicate what the set continues
                    - {p1,p2,p3,...}
                    - imprecise
                    
    When we use 'induction' to show a value 'v' is in a set, we first
    enumerate the values that come before it; these values form a
    'sequence' which is simply a set with an ordering.
    
    Computers can enumerate elements of a set (generate a sequence)
    based on the description of a set. We can implement a chain (sequence)
    using function application.
    
    Consider the following:            Modus Ponens
            1 in S                     If premise,1, is true
            1 in S -> 2 in S           then 2 
            2 in S -> 3 in S           if 2 is true, then 3, etc
            
    which can be implemented as:

        imp1 :: Integer -> Integer
        imp1 1 = 2
        imp1 x = error "premise does not match"
        
        imp2 :: Integer -> Integer
        imp2 2 = 3
        imp2 x = error "premise does not match"
        
        s :: [Integer]
        s = [1, imp1 (s !! 0), imp2 (s !! 1)]
        
        (s !! 0) grabs the '1' in the zero position of the same list
        and passes it to 'imp1', which returns 2.
        (s !! 1) grabs the '2' in the 'one' position of the same list
        and passes it to 'imp2' which returns 3.
        As a result, the value of 's' is [1,2,3]
        
    In each case, the implication relies on the value of the
    starting premise and earlier implications.
    
    If one earlier implication is missing there is no chain.
    If the implications are applied in an illogical order, no chain.
        
-}
{-
    The Induction Rule
    ------------------
    
        We used two premises:
                0 is in S               -- base case
                n -> n + 1 is in S      -- INDUCTION RULE
                
        The induction rule is the case that generates the links
        in the chain.
        
        The 'induction rule' changes with the set we are defining.
        For example,
        
            n in S -> n + 2 in S   -- for even (n=0) or odd (n=1) numbers
            n in S -> n * 5 in S   -- powers of 5
            
        However, constructing the right rule is not always
        straight forward. For example, assume the following:
        
            0 in s
            x in s -> x + 1 in s
            
        Which we can implement as:
            
-}
increment :: Integer -> Integer
increment x = x + 1

s :: [Integer]
s = [0, increment (s !! 0), increment (s !! 1)]

{-
    the above works but if we wanted to know if say, 50, was in the
    set it would be extremely tedious to write it all out; so,
    instead we can define 's' as follows:
-}
sa :: [Integer]
sa = 0 : map increment sa

ex1 = sa !! 50       -- to find out if 50 is in the set

{-
    The above uses 'data recursion' which gives us a new format
    for implementing induction definitions.
    
        1. specify the induction rule
        2. recursively define a list that starts with the base
           case and maps the rule down the list
        
    A set definition must include one more clause, the 'extremal clause'
    which defines what IS NOT IN  the Set. It states, "Nothing is an
    element of the set unless it can be constructed by a finite 
    number of uses of the first two clauses."
   
-}
{-
    The Inductive Definition of a Set
    -----------------------------------

    An inductive definition of a set has three parts:
    
    1.  the base case, which is a simple statement of some mathematical
        fact i.e. 1 in S
        
    2.  the induction case, which is an implication in a general form
        i.e. (all x( in U, x in S -> x + 1 in S.
        
    3.  The extremal clause says that nothing is in the set being defined
        unless it got there by a finite number of uses of the first two 
        cases
        
    Example:
    
    Inductive definition of Natural numbers
    
        Base Case:        0 in N
        Induction Case:   x in N -> x + 1 in N
        Extremal Clause:  nothing is an element of the set N unless it
                          can be constructed with a finite number of
                          uses of the base and induction cases.
                          
    And we can use the rules to show any positive number over 0 is
    in the set of Natural numbers. Here is the proof for the number
    2:
    
        1. 0 in N Base case
        2. 0 in N -> 1 in N        instantiation rule, induction case
        3. 1 in N                  1, 2, Modus Ponens
        4. 1 in N -> 2 in N        instantiation rule, induction case
        5. 2 in N                  3, 4, Modus Ponens    
        
    The set of Natural numbers is said to be 'well founded'.
    
    A 'well founded set' is infinite in one direction and has a 'least'
    number. For the set of Natural numbers, the least number is 0.
    
    A 'countable set' is any set that can be counted using Natural
    numbers.
-}
{-
    The Set of Binary Machine Words
    -------------------------------
    
    Defining a set, BinWords, each of which is a machine word
    represented in binary notation. In general, a machine word
    can be any length.
    
    Let BinDigits be the set {0,1}. The set BinWords in binary
    is defined as:
    
        Base Case:      x in BinDigits -> x in BinWords
        Induction Case: if x is a binary digit and y is a binary word
                        then their concatenation, xy, is a binary word
                        
            (x in BinDigits /\ y in BinWords) -> xy in BinWords
            
        Extremal Clause: nothing is in the set BinWords unless it can be
                         constructed from a finite number of uses of
                         the base and induction cases.
                         
    A Set based on another set is given the notation S^+ indicating it
    is the non-empty set of all possible strings. The notation S^*
    means the same except it includes the empty set our set BinWords
    could be written as BinWords^+
    
    We can use the above to create a Haskell function to generate
    the set of binary words
-}

-- the induction rule takes an existing binary word and creates
-- two new ones by concatenating binary digits
newBinaryWords :: [Integer] -> [[Integer]]
newBinaryWords ys = [0 : ys, 1 : ys]

-- define the set, uses a function to build concatenated BinWords
mappend :: (a -> [b]) -> [a] -> [b]
mappend f []     = []                   
mappend f (x:xs) = f x ++ mappend f xs  

binWords = [0] : [1] : (mappend newBinaryWords binWords)

exBinWords = take 5 binWords

{-
    Defining the set of Integers
    ----------------------------
    
    The set of Integers is NOT well-founded. It has not 'least' number
    and extends infinitely in both two directions (negative and positive).
    
    Is it countable? No, as there is not least number BUT we can use
    a trick to make them countable. We can choose 0 as the the 'least'
    number for both the negative and positive directions. i.e. we can
    count from 0 to n and from 0 to -n.
    
    If we think of the set of Integers as a measuring tape folded on
    the 0 then the positive numbers 'touch' the negative numbers so
    that each element 'i' of the natural numbers counts both the
    positive and negative numbers (i,-i). 
    
    Thinking of Integers this way helps us develop an induction rule
    to define the set of Integers. There are some subtleties however
    so it takes us a few tries:
    
    We can define some functions to help us test out our definitions.
    Both the 'build' and 'builds' functions take the base case and
    the induction case as arguments and generate the set they define.
-}
-- use when adding one element at a time
build :: a -> (a -> a) -> Set a
build a f = set
    where set = a : map f set
    
-- use when adding a 'set' of elements at a time    
builds :: a -> (a -> [a]) -> Set a
builds a f = set    
    where set = a : mappend f set
    
{-
    Attempt 1:
        Base case:          0 in I
        Induction case:     x in I -> -x in I
        Extremal clause:    nothing is in I unless its presence is 
                            justified by a finite number of uses of
                            the base and induction cases
                            
    Fails:
        According to the definition, only 0 is in I.
        There is no rule that will 'generate' new numbers
        
        i.e. the only 'x' is 0 and -0 == 0 so all we get is
             an infinite set of zeros
-}
nextInteger1 :: Integer -> Integer
nextInteger1 x = -x

integers1 :: [Integer]
integers1 = build 0 nextInteger1

ex12 = take 10 integers1   -- [0,0,0,0,0,0,0,0,0,0]

{-
    Attempt 2
        Base Case:      0 in I
        Induction Case: x in I -> (x + 1 in I /\ x - 1 in I)
        Extremal clause:    nothing is in I unless its presence is 
                            justified by a finite number of uses of
                            the base and induction cases
                            
    Fail:
        The induction rule appears to be correct as it does generate
        all the positive and negative numbers but there is still 
        a problem which can be seen with this simple example for
        proving that -2 is in the set:
        
        1. 0 in I                         base case
        2. 0 in I ->(1 in I && −1 in I)   instantiation, induction case
        3. 1 in I && −1 in I              1,2, Modus Ponens
        4. −1 in I -> (0 in I && −2 in I) instantiation, induction case
        5. 0 in I && −2 in I              3,4, Modus Ponens
        
        The problem is that the 'base case', 0, is being generated
        by the induction rule so that it appears more than once which
        is easily seen if you run the code or walk through it
        
          [0]                           ++ [  0 + 1,  0 - 1]
        = [0,1,-1]                      ++ [  1 + 1,  1 - 1]
        = [0,1,-1,2,0]                  ++ [ -1 + 1, -1 - 1]
        = [0,1,-1,2,0,0,-2]             ++ [  2 + 1,  2 - 1]
        = [0,1,-1,2,0,0,-2,3,1]         ++ [  0 + 1,  0 - 1]
        = [0,1,-1,2,0,0,-2,3,1,1,-1]    ++ ....
        
        Need to find an induction rule that will not generate the
        base case, or generate any other element more than once
        (remember, there are no duplicated elements in a Set)
-}
nextIntegers2 :: Integer -> [Integer]
nextIntegers2 x = [x + 1, x - 1]

integers2 :: [Integer]
integers2 = builds 0 nextIntegers2

ex13 = take 20 integers2 -- [0,1,-1,2,0,0,-2,3,1,1,-1,1,-1,-1,-3,4,2,2,0,2]

{-
    Attempt 3
        Base Case:          0 in I
        Induction Case:     x in I -> (x+1 in I /\ -(x+1) in I)
        Extremal clause:    nothing is in I unless its presence is 
                            justified by a finite number of uses of
                            the base and induction cases
        
    Walk through  code:
        [0]                 ++ [  0 + 1, -(0 + 1) ]
        [0,1,-1]            ++ [  1 + 1, -(1 + 1) ]
        [0,1,-1,2,-2]       ++ [ -1 + 1, -(-1 + 1) ]
        [0,1,-1,2,-2,0,0]   ++  ...
        
    So it looks like we are still not there.  We have a correct
    definition (it describes all the Integer numbers) but we
    need some way to correctly increment and decrement x.
-}
nextIntegers3 :: Integer -> [Integer]
nextIntegers3 x = [x + 1, -(x + 1)]

integers3 :: [Integer]
integers3 = builds 0 nextIntegers3

ex14 = take 10 integers3    -- [0,1,-1,2,-2,0,0,3,-3,-1]

{-
    Attempt 4
        Base Case:          0 in I
        Induction Case:     (x in I /\ x >= 0) -> x + 1 in I
                            (x in I /\ x <  0) -> x - 1 in I
        Extremal clause:    nothing is in I unless its presence is 
                            justified by a finite number of uses of
                            the base and induction cases
                            
    Walk through code:
    
        [0]                     ++ [ 0 + 1 ]
        [0,1]                   ++ [ 1 + 1 ]
        [0,1,2]                 ++ [ 2 + 1 ]
        [0,1,2,3]               ++ ....
        
    Still not there, we are getting the positive numbers but
    not the negative ones.
                            
-}
nextInteger4 :: Integer -> Integer
nextInteger4 x = if x < 0 then x - 1 else x + 1

integers4 :: [Integer]
integers4 = build 0 nextInteger4

ex15 = take 10 integers4    -- [0,1,2,3,4,5,6,7,8,9]

{-
    Attempt 5
        Base Case:          0 in I
        Induction Case:     (x in I /\ x >= 0) ->   x + 1 in I
                                               /\ -(x + 1) in I
        Extremal clause:    nothing is in I unless its presence is 
                            justified by a finite number of uses of
                            the base and induction cases
    
    Walk through code:
        [0]                 ++  0 == 0 so [ 0 + 1, -(0 + 1)]
        [0,1,-1]            ++  1 >  0 so [ 1 + 1, -(1 + 1)]
        [0,1,-1,2,-2]       ++ -1 <  0 so []
        [0,1,-1,2,-2]       ++  2 >  0 so [ 2 + 1, -(2 + 1)]
        [0,1,-1,2,-2,3,-3]  ++ ....
        
    This appears to be working correctly, we're skipping the elements
    that are less than 1 so not generating additional zeros or duplicate
    elements.
    
-}
nextIntegers5 :: Integer -> [Integer]
nextIntegers5 x =
    if    x > 0 \/ x == 0
    then [x + 1, -(x + 1)]
    else []

integers5 :: [Integer]
integers5 = builds 0 nextIntegers5

ex16 = take 10 integers5    -- [0,1,-1,2,-2,3,-3,4,-4,5]

