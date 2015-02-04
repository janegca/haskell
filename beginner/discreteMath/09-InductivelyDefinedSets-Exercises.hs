-- Chapter 9 - Inductively Defined Sets - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm

{-
    Exercise 1
        Is the following a chain? You can test your conclusions by 
        evaluating s in each case.
        
        Yes; the implications all follow from one another
-}
imp1 :: Integer -> Integer
imp1 1 = 2
imp1 x = error "imp1: premise does not apply"

imp2 :: Integer -> Integer
imp2 2 = 3
imp2 x = error "imp2: premise does not apply"

imp3 :: Integer -> Integer
imp3 3 = 4
imp3 x = error "imp3: premise does not apply"

s :: [Integer]
s = [1, imp1 (s !! 0), imp2 (s !! 1), imp3 (s !! 2)]

{-
    Exercise 2
        Is the following a chain?
        
        No; missing an implication, there is nothing to imply
        2 leads to 3 or that 0 implies 1
-}
imp1a :: Integer -> Integer
imp1a 1 = 2
imp1a x = error "imp1: premise does not apply"

imp2a :: Integer -> Integer
imp2a 3 = 4
imp2a x = error "imp2: premise does not apply"

sa :: [Integer]
sa = [0, imp1a (sa !! 0), imp2a (sa !! 1)]

{-
    Exercise 3
        Is the following a chain?
        
        No. Nothing from which to infer 3.
        
-}
imp1b :: Integer -> Integer
imp1b 0 = 1
imp1b x = error "imp1: premise does not apply"

imp2b :: Integer -> Integer
imp2b 3 = 4
imp2b x = error "imp2: premise does not apply"

sb :: [Integer]
sb = [0, imp1b (sb !! 0), imp2b (sb !! 1)]

{-
    Exercise 4
        Is the following a chain?
        
        No. Implications are right but applied incorrectly in the
        list comprehension.
-}
imp1c :: Integer -> Integer
imp1c 0 = 1
imp1c x = error "imp1: premise does not apply"

imp2c :: Integer -> Integer
imp2c 1 = 2
imp2c x = error "imp2: premise does not apply"

sc :: [Integer]
sc = [0, imp1c (sc !! 1), imp2c (s !! 0)]

-- INDUCTION RULE -------------------------------------------------------
{-  
    Exercise 5
    
    Given the base case 0 ∈ n and the induction rule x ∈ n → x+1 ∈
    n, fix the following calculation so that 3 is in set n  

        fun :: Integer -> Integer
        fun x = x - 1
        
        n :: [Integer]
        n = 0 : map fun n    
-}
fun :: Integer -> Integer
fun x = x + 1       -- rule was implemented incorrectly

n :: [Integer]
n = 0 : map fun n

ex5 = n !! 3

{-
    Exercise 6
    
    Use the following definitions, determine whether 4 is in set s,
    given 1 ∈ s and the induction rule x ∈ s → x + 2 ∈ s.
    
    Ans: If we start from 1 and always add 2 we will only get
         odd numbers so 4 won't be in the set.
         
         (Note:Is there a simple way to check this with the computer??)
-}
fun6 :: Integer -> Integer
fun6 x = x + 2

s6 :: [Integer]
s6 = 1 : map fun6 s6

ex6 = s6 !! 4             -- an inadequate check as numbers not
                          -- sequential (indexes do not = x values)
--ex6 = elem 4 s6         -- overflow

{-
    Exercise 7
        Fix this calculation of the positive integers.
        
            fun :: Integer -> Integer
            fun x = 0

            p :: [Integer]
            p = 0 : map fun p            
-}
fun7 :: Integer -> Integer
fun7 x = x + 1      -- if x = 0 then all values will be zero

p :: [Integer]
p = 0 : map fun7 p

{-
    Exercise 8
    
    Fix this calculation of the positive multiples of 3:
    
        fun :: Integer -> Integer
        fun x = x * 3
        
        p :: [Integer]
        p = map fun p    
-}
fun8 :: Integer -> Integer
fun8 x = x * 3

p8 :: [Integer]
p8 = 3 : map fun8 p8    -- wasn't in list format

ex8 = take 5 p8

{-
    Exercise 9
        Here is a Haskell equation that defines the set s inductively. 
        Is 82 an element of s?
        
        Yes. 
            Base case:         0
            Induction rule is: x + 2, 
            
            Defines even numbers, 82 is an even number.
-}
s9 :: [Integer]
s9 = 0 : map ((+) 2) s9

ex9 = elem 82 (take 50 s9)      -- True

{-
    Exercise 10
        What set is defined by the following?
        
        Powers of 3
            Base Case:      1
            Induction Case: x * 3
-}
s10 :: [Integer]
s10 = 1 : map ((*) 3) s10

ex10 = take 10 s10

{-
    Exercise 11
        Alter the definition of newBinaryWords and binWords so that
        they produce all of the octal numbers. An octal number is one 
        that contains only the digits 0 through 7.
-}
-- given an octal word, generate a new one
newOctalWords :: [Integer] -> [[Integer]]
newOctalWords ys = [ x:ys | x <- [0..7] ]
                    --[0 : ys, 1 : ys, 2 : ys, 3 : ys,
                    --4 : ys, 5 : ys, 6 : ys, 7 : ys]

-- add an element to a set                    
mappend :: (a -> [b]) -> [a] -> [b]
mappend f []     = []                   
mappend f (x:xs) = f x ++ mappend f xs  

octalWords = [0] : [1] : [2] : [3] : [4] : [5] : [6] : [7]
                 : (mappend newOctalWords octalWords)

ex11 = take 100 octalWords

-- Ex 12 - 16 - see 09InductivelyDefinedSets.hs

-- REVIEW EXERCISES ------------------------------------------------------

-- two functions to assist in inductively building sets
-- use when adding one element at a time
build :: a -> (a -> a) -> Set a
build a f = set
    where set = a : map f set
    
-- use when adding a 'set' of elements at a time    
builds :: a -> (a -> [a]) -> Set a
builds a f = set    
    where set = a : mappend f set

{-
    Exercise 17
        Does ints, using the following definition, enumerate the 
        integers? If it does, then you should be able to pick any
        integer and see it eventually in the output produced by ints. 
        Will you ever see the value -1?
        
    Walk through nats:
        [0]             (1+) 0 
        [0,1]           (1+) 1
        [0,1,2]         (1+) 2
        [0,1,2,3]       ....
        
    Walk through negs:
        [-1]            (1-) -1
        [-1,-2]         (1-) -2
        [-1,-2,-3]      ...
     
    The functions will generate all the Integers BUT
    Both nats and negs generate infinite lists; as we are trying
    to append two infinite lists we will never see the values
    generated by 'negs'
    
-}
nats :: [Integer]
nats = build 0 (1 +)

negs :: [Integer]
negs = build (-1) (1 -)

ints :: [Integer]
ints = nats ++ negs

{-
    Exercise 18
        Does twos enumerate the set of even natural numbers?
        
        Nope. It will generate a list of zeros.
-}
twos' :: [Integer]
twos' = build 0 (2 *)    

ex18 = take 10 twos'     -- [0,0,0,0,0,0,0,0,0,0]

{-
    Exercise 19
        What is wrong with the following definition of the stream of
        natural numbers?
        
        nats = map (+ 1) nats ++ [0]
        
        Ans: we are trying to map over 'nats ++ [0]' but 'nats'
             is a function and has no initial value
-}
nats19 = map (+ 1) nats19 ++ [0]
ex19   = take 10 nats19     -- ERROR - C stack overflow

{-
    Exercise 20
        What is the problem with the following definition of the naturals?
        
    Ans: Naturals will never terminate.
    
    
-}

naturals :: [Integer] -> [Integer]
naturals (i:acc) = naturals (i + 1:i:acc)

nats20 :: [Integer]
nats20 = naturals [0]       -- ERROR - C stack overflow

{-
    Exercise 21
        Can we write a function that will take a stream of the naturals
        (appearing in any order) and give the index of a particular 
        number?
        
    Ans: No, not if they appear 'in any order'
-}
{-
    Exercise 22
        Using induction, define the set of roots of a given number n.
        
    Then roots of a number are given by, for example, square root of n,
    cubed root of n, etc. And every number has 1 as a so {1,2,3,...}
    
    Provided Solution:
        Base Case:          n^1 
        Induction Case:     n^(1/m) in R -> n^(1/m+1) in R
        Extremal Clause:    Nothing is in R unless it can be shown to be 
                            in R by a finite number of uses of the base 
                            and induction rules.
                            
-}
vals = [ 1/m | m <- [1..]]

ex22 n = take 10 (map (n**) vals)

{-
    Exercise 23
    
    Given the following definition, prove that n^3 is in set P of powers
    of n.
    
    Definition 28. Given a number n, the set P of powers of n is defined
    as follows:
        • n^0 in P
        • n^m in P -> n^(m+1) in P
        • Nothing else is in P unless it can be shown to be in P by a 
          finite  number of uses of the base and induction rules. 

    Ans:
    
        1. n^0 in P                       base case
        2. n^0 in P -> (n^(0+1) in P)     induction case
        3. n^1 in P                       1,2, Modus Ponens
        4. n^1 in P -> (n^(1+1) in P)     induction case
        5. n^2 in P                       3,4, Modus Ponens
        6. n^2 in P -> (n^(2+1) in P)     induction case
        7. n^3 in P                       5,6 Modus Ponens

-}
{-
    Exercise 24
    
    When is 0 in the set defined below?
    
    Definition 29. Given a number n, the set N is defined as follows:
    
        • n in N
        • m in N -> m - 2 in N
        • Nothing is in N unless it can be shown to be in N by a finite 
          number of uses of the previous rules.    
    
    Provided Solution:
        If n is a positive multiple of 2 then yes, otherwise no.
        
    Why??
    
        There is nothing to connect n and m. Are we to assume m = n
        initially (a base case value?)??
        
        Try to prove for 0 is in the set if n is 2:
        
        1. 2 in N
        2. 2 in N -> 2 - 2 in N
        3. 0 in N                   -- proven
        4. 0 in N -> 0 - 2 in N
        5. ....
        
        And if n is 1
        
        1. 1 in N
        2. 1 in N -> 1 - 2 in N
        3. -1 in N
        4. -1 in N -> -1 - 2 in N
        5. -3 in N
        6 ....                      -- proof fails
        
        What about 4?
        
        1. 4 in N
        2  4 in N -> 4 - 2 in N
        3. 2 in N -> 2 - 2 in N
        4. 0 in N                   -- proven
        5. ....
        
        Ah, ok, so with every even number we eventually reduce to
        0; with odd numbers, we skip over 0.
        
-}
{-
    Exercise 25
    
    What set is defined by the following definition?
    
    Definition 30. The set S is defined as follows:
    
        • 1 in S
        • n in S /\ nmod2 = 0 -> n + 1 in S
        • n in S /\ nmod2 = 1 -> n + 2 in S
        • Nothing else is in S unless it can be shown to be in S by a 
          finite number of uses of the previous rules.    

    Ans: 
        1. 1 in S                                   -- base
        2. 1 in S && nmod2 = 1 -> 1 + 2 in S        -- induction
        3. 3 in S                                   -- 1,2 modus ponens
        4. 3 in S && nmod2 = 1 -> 3 + 2 in S        -- induction
        5. 5 in S                                   -- 3,4 modus ponens
        6. 5 in S ....
        
        All the odd numbers.
-}
--
-- NOTE ON BASE CASE VALUES:
--      Based on the last 2 exercises, it appears the value of
--      a base case can be 'fixed' (supplied as part of the code)
--      OR passed in as an argument 
--
{-
    Exercise 26
    
    Prove that 4 is in the set defined as follows:
    
    Definition 31. The set S is defined as follows:
        1. 0 in S
        2. n in S /\ nmod2 = 0 -> n + 2 in S
        3. n in S /\ nmod2 = 1 -> n + 1 in S
        4. Nothing is in S unless it can be shown to be in S by a finite
           number of uses of the previous rules.
           
    Ans:
        1. 0 in S                               -- base
        2  0 in S /\ nmod2 = 0 -> 0 + 2 in S    -- induction
        3. 2 in S                               -- 1,2 modus ponens
        4. 2 in S /\ nmod2 = 0 -> 2 + 4 in S    -- induction
        5. 4 in S                               -- 3,4 modus ponens
        6. 4 in S /\ ...
        
        Therefore 4 is in the set
        
-}
{-
    Exercise 27
    
    Given the following definition, prove that the string "yyyy" is in
    YYS.
    
    Definition 32. The set YYS of strings containing pairs of the letter
        'y' is defined as follows:
        
        1. "" in YYS
        2. s in YYS -> "yy" ++ s in YYS
        3. Nothing else is in YYS unless it can be shown to be in YYS
           by a finite number of uses of rules (1) and (2).
           
    Ans:
        1. "" in YYS                            -- base case
        2. "" in YYS -> "yy" ++ "" in YYS       -- induction
        3. "yy" in YYS                          -- 1,2 modus ponens
        4. "yy" in YYS -> "yy" ++ "yy" in YYS   -- induction
        5. "yyyy" in YYS                        -- 3,4 modus ponens
        
        Therefore "yyyy" is in YYS

-}
{-
    Exercise 28
    
    Using data recursion, define the set of strings containing the
    letter ‘z’.
-}
setZ :: [String]
setZ = "" : map ('z' : ) setZ

ex28 = take 20 setZ

{-
    Exercise 29
    
    Using induction, define the set of strings of spaces of length less
    than or equal to some positive integer n.
    
    Ans:
    
    The set SPC, containing strings of spaces of lengths
    less than or equal to some positive number n, is defined as follows:
    
    1. "" in SPC
    2. s  in SPC /\ length s < n -> ' ' : s in SPC
    3. Nothing else is in SPC unless it can be shown to be in SPC by a
       finite number of uses of rules (1) and (2).

-}

{-
    Exercise 30
    
    Using recursion, define the set of strings of spaces of length less
    than or equal to length n, where n is a positive integer.
-}
spaces :: Int -> [String]
spaces n = take n spcs
    where
        spcs = "" : map (' ' : ) spcs   -- recursive
        
ex30 = spaces 10

-- provided solution
ss :: Int -> [String]
ss 0 = []
ss n = take n (repeat ' ') : ss (n-1)

ex30a = ss 10

{-
    Exercise 31
    
    We could have a set that consists of all the natural numbers
    except for 2; you can write this as N - {2}. Similarly, for every 
    natural number x, there is a set that contains all the natural 
    numbers except for x. Now, we could make a set SSN of all of these 
    results. Write an inductive definition of SSN.
    
    Ans:
    
    The set SSN, containing all naturals numbers except x, is
    defined as follows:
    
    1. 0 in SSN                             <-- MISTAKE, See prov.soln.
    2. n in SSN /\ n /= x -> n + 1 in SSN
    3. n in SSN /\ n == x -> n + 2 in SSN
    4. Nothing else is in SSN unless it can be shown to be in SSN by a
       finite number of uses of rules (1), (2) and (3).
    
    Provided Solution:

    The set of sets of naturals SSN, each of which is missing a distinct 
    natural number, is defined inductively as follows:
    
    1.  N - {0} in SSN
    2. (N - {n})in SSN -> (N - {n + 1} in SSN
    3. Nothing is in SSN unless it can be shown to be in SSN by a finite 
       number of uses of rules (1) and (2).    
       
-}
{-
    Exercise 32
    
    Given the following definition, show that the set I - {−3} in
    SSI-.
    
    The set of sets of integers SSI, each of which is missing a distinct 
    negative integer, is defined inductively as follows:
    
    1. I - {−1} in SSI−
    2. I - {n} -> I - {n - 1} in SSI−
    3. Nothing else is in SSI- unless it can be shown to be in SSI- by 
       a finite number of uses of rules (1) and (2).
       
    Ans:
    
    1. I - { -1} in SSI-                         -- base case
    2. I - { -1} -> I - { -1 - 1} in SSI-         -- induction             
    3. I - { -2} in SSI-                         -- 1,2, MP
    4. I - { -2} in SSI- -> I - { -2 - 1} in SSI-  -- induction
    5. I - { -3) in SSI                          -- 3,4, MP
    
-}
{-
    Exercise 33
    
    Given the following definition, prove that -7 is in ONI. The set
    ONI of odd negative integers is defined as follows:
    
    1. −1 in ONI
    2. n in ONI -> n - 2 in ONI
    3. Nothing is in ONI unless it can be shown to be in ONI by a 
       finite  number of uses of the previous rules.
       
    Ans:
    
    1. -1 in ONI                            -- base case
    2. -1 in ONI -> -1 - 2 in ONI           -- induction
    3. -3 in ONI                            -- 1,2, MP
    4. -3 in ONI -> -3 - 2 in ONI           -- induction
    5. -5 in ONI                            -- 3,4 MP
    6. -5 in ONI -> -5 - 2 in ONI           -- induction
    7. -7 in ONI                            -- 5,6 MP
    
    Therefore -7 is in ONI
-}
{-
    Exercise 34
    
    Using data recursion, define the set ni of negative integers
-}

ni = -1 : map ( + (-1) ) ni

ex34 = take 10 ni

{-
    Exercise 35
    
    If you print the elements of
    
        [(a,b) | a <- [0..], b <- [0..]]
        
    will you ever see the element (1,2)?
    
    Ans:
        No. The comprehension sets up as an inner/outer loop
        so you'll get [(0,1),(0,2),(0,3),...] into infinity
        
-}
{-
    Exercise 36
    
    What set is given by the following definition?
    
    Definition 33. The set S is defined as follows:
    1. 1 in S
    2. n in S -> n - n in S
    3. Nothing is in S unless it can be shown to be in S by a finite 
       number of uses of the previous rules

    Ans: 
    
    1. 1 in S
    2. 1 in S -> 1 - 1 in S so 0
    3. 0 in S -> 0 - 0 in S so 0
    4.  ....
    
    So the set contains the base case 1, and the induction value 0
    or {0,1}.
    
-}
