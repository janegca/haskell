{-
    Chapter 2 - Talking About Mathematical Objects
    
    logical validities
        formulas that receive the value 't' regardless of the values
        of P,Q, etc. e.g. P => P, P || not P, P => (Q => P)
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
{-# LANGUAGE FlexibleInstances #-}
module TAMO where
{-
    TRUTH TABLES
    
        'Connective' symbols
        
        and         /\      conjunction     built-in op:     &&                       
        or          \/      disjunction     built-in op:     ||
        not                 negation        built-in op:     not
        if-then     ->      implication     not built in
        iff         <=>     equivalence     not built in
        
        Related tables:
        
        P  Q  P && Q    P  Q  P || Q        P  (not P)
        ------------    ------------        ----------
        T  T     T      T  T    T           T     F
        T  F     F      T  F    T           F     T
        F  T     F      F  T    T
        F  F     F      F  F    F

        P  Q  P -> Q        P  Q  P <=> Q
        ------------        -------------
        T  T    T           T  T     T
        T  F    F           T  F     F
        F  T    T           F  T     F
        F  F    T           F  F     T
-}
-- defining operators
--      binding '9' is highest, '0' is lowest

--
-- the 'implication' operator
--
infix 1 ==>
 
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

--
-- the 'equivalence' operator
--
infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

--
-- the 'exclusive or' (xor) operator
--
--      P   Q   P <+> Q
--      ---------------         exclusive or is False
--      T   T      F            if both values are True
--      T   F      T            otherwise follows normal
--      F   T      T            'disjunct' (Or) values
--      F   F      F            i.e. is 'either-or' rather than 'or'
--
infix 1 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

{-
  Example of using truth tables if P and Q are given values True and False

    not P && ((P => Q) <=> not (Q && not P))
      . t  .   t .  f   .   .   f  .  .  t       substitute P and Q values
      f    .     f      .             f          order of operations
           .            .          f             order of operations
           .            .   t                    order of operations
           .            f                        order of operations
           f                                     final result                    


-}
p = True
q = False

formula1 :: Bool
formula1 = (not p) && (p ==> q) <=> not (q && (not p))

{-
    Creating a truth table for a propositional formula
        1. assign all possible values to P and Q
        2. use order of operations to complete the table
        
     Ex P => (Q => P)
     
         Step 1              Inner impl           Outer impl
                          (result under op)   (result under op)
        P => (Q => P)       P => (Q => P)       P => (Q => P)
        -------------       -------------       -------------
        t     t    t        t     t  T t        t  T  t  T  t
        t     f    t        t     f  T t        t  T  f  T  t
        f     t    f        f     t  F f        f  T  t  F  f
        f     f    t        f     f  T f        f  T  f  T  f
                            ------------        -------------
                                  c  r c        c  r     c     c=col, r=res
       
    therefore, P => (Q => P) is a 'logical validity', no matter
    the values of P,Q, the result is ALWAYS TRUE
    
    The 'truth table method of checking for validity' involves
    checking if a propositional function yields True for every
    possible combination of variable values.
-}

-- formula2 is a 'propositional' or 'Boolean' or 'truth' function
-- it takes the values of P and Q as arguments, processes them,
-- and produces a result
formula2 :: Bool -> Bool -> Bool
formula2 p q = (not p) && (p ==> q) <=> not (q && (not p))

-- checking propositional functions with one argument
valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && (bf False)

-- excluded middle (tertium non datur): P or (not P)
excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

exValid1 = valid1 excluded_middle       -- returns True, so excluded_middle
                                        -- is a 'valid' propositional
                                        -- formula
                                        
-- check the validity of propositional formulas that take 2 arguments
-- checks all combinations of True and False for P and Q
valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = (bf True  True)
         && (bf True  False)
         && (bf False True)
         && (bf False False)         

form1 p q = p ==> (q ==> p)
form2 p q = (p ==> q) ==> p

exV21 = valid2 form1    -- True, therefore valid
exV22 = valid2 form2    -- False, therefore not valid
exV23 = valid2 formula2 -- False, therefore not valid

-- we can use Haskell list comprehensions to generate the required
-- number of 'true,false' combinations to check propositional
-- formulas taking 3 or more arguments
-- [Note:  the vars p,q,r,s are generated in the same manner
--         as 'loop' variables
--         i.e. [(p,q) | p <- [True,False], q <- [True, False]]
--              produces
--              [(True,True),(True,False),(False,True),(False,False)]
--              
valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [True, False],
                             q <- [True, False],
                             r <- [True, False]]
                             
valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [ bf p q r s | p <- [True, False],
                               q <- [True, False],
                               r <- [True, False],
                               s <- [True, False]]
                               
{-
    Operator Precedence
        /\ and \/ bind more strongly than
        => and <=>
        
        In Haskell, (highert the precedence, stronger the binding)
            ==      binding precedence is 4
            &&      binding precedence is 3
            ||      binding precedence is 2
            
            and the two operators we've created            
                =>  <=>             
            both have a binding precedence of 1
            
        i.e.    P /\ Q => R  equals (P /\ Q) => R, NOT P /\ (Q => R)
        
-}       
{-
    Logical Equivalence
        Two formulas are logically equivalent if, given the same
        truth values, the produce the same result.
        
        An example is the 'First Law of De Morgan'
            not (P && Q) <=> (not P || not Q)                        
-}                 
--
-- Exercise 2.9
--      Show that (P xor Q) xor Q is equal to P
{-
        (P  <+>  Q)  <+> Q   ==  P
        --------------------------      computed column 4 is
        t    F   t    T  t       t      equal to last column,
        t    T   f    T  f       t      original value of P
        f    T   t    F  t       f
        f    F   f    F  f       f
        
-}       
--
-- checking for the logical equivalence of propositional functions
--
logEquiv1 :: (Bool -> Bool)   -- propositional function with 1 arg
          -> (Bool -> Bool)   -- propositional function with 1 arg
          -> Bool             -- result
logEquiv1 bf1 bf2 = (bf1 True  <=> bf2 True)
                 && (bf1 False <=> bf2 False)
                 
logEquiv2 :: (Bool -> Bool -> Bool) -- prop. function with 2 args
          -> (Bool -> Bool -> Bool) -- prop. function with 2 args     
          -> Bool                   -- result
logEquiv2 bf1 bf2 = and [ (bf1 p q) <=> (bf2 p q) | p <- [True,False],
                                                    q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -- prop. function with 3 args
          -> (Bool -> Bool -> Bool -> Bool) -- prop. function with 3 args     
          -> Bool                           -- result
logEquiv3 bf1 bf2 = and [ (bf1 p q r) <=> (bf2 p q r) | p <- [True,False],
                                                        q <- [True,False],
                                                        r <- [True,False]]
                                                                                                               
                                                        
-- example usage
formula3 p q = p
formula4 p q = (p <+> q) <+> q
                                                        
le1 = logEquiv2 formula3 formula4       -- result is True

formula5 p q = p <=> ((p <+> q) <+> q)
v2 = valid2 formula5

{-
    'logical equivalence' checks if two 'formulas' are equivalent
    'equivalence'(<=>) is simply an operator used in formulas
    
    i.e. 'le1' above is a 'statement' about the relation between
               the two formulas bf1 and bf2
          formula5 is just another formula
         
-}

-- a type class for a truth function
--      if a type 'p' is a truth function (TF) it must define
--      functions for validity (valid) and logical equivalence (lequiv)
class TF p where
    valid   ::  p -> Bool
    lequiv  ::  p -> p -> Bool
    
-- an 'instance' for a truth function with no parameters
--      a truth function with now parameters is a constant, either
--      True or False
instance TF Bool
    where
        valid      = id             -- identity function
        lequiv f g = f == g         -- returns result of the comparison
        
-- an 'instance' for a 'truth function'
-- if we know 'p' is a TF (a truth function) then (Bool -> p) is 'the
-- type' of a truth function
-- and so the other types can be defined by recursion
-- i.e. since 'p' is a TF we know we have the methods 'valid' and
--      'lequiv' for 'p' and so we can call on them        
instance TF p => TF (Bool -> p)
    where
        valid  f   = valid (f True) && valid (f False)
        lequiv f g = (f True) `lequiv` (g True)
                  && (f False) `lequiv` (g False)
            
-- Theorem 2.10 useful equivalences
--      the tests (below) are written using 'lambda abstractions'
--      these are 'anonymous' (unnamed functions) of the form:
--          \       lambda operator indicating start of the function
--          p       function argument (can be more than 1)
--         ->       beginning of the function body
--                  i.e. everything after the '->' is the function body
--      So,
--          (\ p -> not (not p))
--      is a short cut (syntactic sugar) for writing
--
--          doubleNegation :: Bool -> Bool
--          doubleNegation p = not (not p))
--
--  All the tests evaluate to True and can be shown to be True
--  by manually creating truth tables for each

-- law of double negation -----------------------------------------------
{-  Note: 'id', used in the tests below, is a built-in identity function
          i.e. it always returns its argument
                    id p = p
                    
        P     not (not P)      col 1 is equal to the computed col 2
        ---  ------------
        t      T    F   t
        f      F    T   f
-}
test1  = lequiv id (\ p -> not (not p))

-- laws of idempotence
{-
    P   P && P           P  P || P
   ---  ------          --- ------     col 1 = computed col 3
    t   t  T t           t  t  T t     in both tables
    f   f  F f           f  f  F f
   
-}
test2a = lequiv id (\ p -> p && p) 
test2b = lequiv id (\ p -> p || p) 

-- laws of contraposition
{-
    In the following, the columns in each table that are marked below
    with an '=', agree and show the truth of the equivalence
    
         Test3a             |              Test3b
    P => Q   (not P) || Q   |   not(P => Q)     P && (not Q)
    ------   ------------   |   -----------     ------------
    t  T t     F  t  T  t   |    F  t T  t      t  F   F  t
    t  F f     F  t  F  f   |    T  t F  f      t  T   T  f
    f  T t     T  f  T  t   |    F  f T  t      f  F   F  t
    f  T f     T  f  T  f   |    F  f T  f      f  F   T  f
       =             =      |    =                 =
       
                     Test4a       |             Test4b
    (not P) => (not Q)   Q => P   |  P => (not Q)    Q => (not P)
    ------------------   ------   |  ------------    ------------
      F  t   T   F  t    t T  t   |  t  F   F  t     t  F   F  t
      F  t   T   T  f    f T  t   |  t  T   T  f     f  T   F  t
      T  f   F   F  t    t F  f   |  f  T   F  t     t  T   T  f
      T  f   T   T  f    f T  f   |  f  T   T  f     f  T   T  f
             =             =      |     =               =
             
               Test4c             
    (not P) => Q    (not Q) => P   
    ------------    ------------
      F  t  T  t      F  t  T  t
      F  t  T  f      T  f  T  t
      T  f  T  t      F  t  T  f
      T  f  F  f      T  f  F  f
            =               =
-}
test3a = lequiv (\ p q -> p ==> q) (\ p q -> not p || q)
test3b = lequiv (\ p q -> not (p ==> q)) (\ p q -> p && not q)

test4a = lequiv (\ p q -> not p ==> not q) (\ p q -> q ==> p)
test4b = lequiv (\ p q -> p ==> not q) (\ p q -> q ==> not p)
test4c = lequiv (\ p q -> not p ==> q) (\ p q -> not q ==> p)

-- laws of commutativity
{-
          Test5a                   |        Test5b
    P <=> Q    P => Q  &&  Q => P  |  P <=> Q   P && Q || (not P)&&(not Q)
    -------    ------------------  | --------   --------------------------
    t  T  t    t T  t  T   t T  t  | t  T   t   t  T t T   F   t F  F   t
    t  F  f    t F  f  F   f T  f  | t  F   f   t  F f F   F   t F  T   f
    f  F  t    f T  t  F   t F  f  | f  F   t   f  F t F   T   f F  F   t
    f  T  f    f T  f  T   f T  f  | f  T   f   f  F f T   T   f T  T   f
       =               =           |    =              =
       
          Test6a       |
    P &&  Q    Q && P  |  P || Q    Q || P
    -------    ------  |  ------    ------
    t  T  t    t  T t  |  t  T t    t  T t
    t  F  f    f  F t  |  t  T f    f  T t
    f  F  t    t  F f  |  f  T t    t  T f
    f  F  f    f  F f  |  f  F f    f  F f
       =          =    |     =         =
-}
test5a = lequiv (\ p q -> p <=> q) 
                (\ p q -> (p ==> q) && (q ==> p))
test5b = lequiv (\ p q -> p <=> q) 
                (\ p q -> (p && q) || (not p && not q))
                
test6a = lequiv (\ p q -> p && q) (\ p q -> q && p)
test6b = lequiv (\ p q -> p || q) (\ p q -> q || p)

-- DeMorgan Laws
{-
             Test7a                  |          Test7b
    not(P && Q)   (not P) || (not Q) | not(P || Q)   not P && not Q
    -----------   ------------------ | -----------   --------------
    F   t  T t      F  t   F   F  t  |  F  t T  t     F  t  F  F  t
    T   t  F f      F  t   T   T  f  |  F  t T  f     F  t  F  T  f
    T   f  F t      T  f   T   F  t  |  F  f T  t     T  f  F  F  t
    T   f  F f      T  f   T   T  f  |  T  f F  f     T  f  T  T  f
    =                      =         |  =                   =

-}
test7a = lequiv (\ p q -> not (p && q)) 
                (\ p q -> not p || not q)
test7b = lequiv (\ p q -> not (p || q)) 
                (\ p q -> not p && not q)
                
-- laws of associativity 
{-
               Test8a               |             Test8b
    P && (Q && R)   (P && Q) && R   |  P || (Q || R)   (P || Q) || R
    ------------    -------------   |  -------------   -------------
    t  T  t T  t    t  T  t  T  t   |  t  T  t  T t    t  T  t  T  t
    t  F  t F  f    t  T  t  F  f   |  t  T  t  T f    t  T  t  T  f
    t  F  f F  t    t  F  f  F  t   |  t  T  f  T t    t  T  f  T  t
    t  F  f F  f    t  F  f  F  f   |  t  T  f  F f    t  T  f  T  f
    f  F  t T  t    f  F  t  F  t   |  f  T  t  T t    f  T  t  T  t
    f  F  t F  f    f  F  t  F  f   |  f  T  t  T f    f  T  t  T  f
    f  F  f F  t    f  F  f  F  t   |  f  T  f  T t    f  F  f  T  t
    f  F  f F  f    f  F  f  F  f   |  f  F  f  F f    f  F  f  F  f
       =                     =      |     =                     =

-}           
test8a = lequiv (\ p q r -> p && (q && r)) 
                (\ p q r -> (p && q) && r)
test8b = lequiv (\ p q r -> p || (q || r)) 
                (\ p q r -> (p || q) || r)
                
-- distribution laws
{-
               Test9a
    P && (Q || R)   (P && Q) || (P && R)
    -------------   --------------------
    t T   t T  t     t T  t   T  t T  t
    t T   t T  f     t T  t   T  t F  f
    t T   f T  t     t F  f   T  t T  t
    t F   f F  f     t F  f   F  t F  f
    f F   t T  t     f F  t   F  f F  t
    f F   t T  f     f F  t   F  f F  f
    f F   f T  t     f F  f   F  f F  t
    f F   f F  f     f F  f   F  f F  f
      =                       =
      
               Test9b
    P || (Q && R)   (P || Q) && (P || R)
    -------------   --------------------
    t T   t T  t     t T  t   T  t  T t
    t T   t F  f     t T  t   T  t  T f
    t T   f F  t     t T  f   T  t  T t
    t T   f F  f     t T  f   T  t  T f
    f T   t T  t     f T  t   T  f  T t
    f F   t F  f     f T  t   F  f  F f
    f F   f F  t     f F  f   F  f  T t
    f F   f F  f     f F  f   F  f  F f
      =                       =
    
-}           
test9a = lequiv (\ p q r -> p && (q || r)) 
                (\ p q r -> (p && q) || (p && r))
test9b = lequiv (\ p q r ->  p || (q && r)) 
                (\ p q r -> (p || q) && (p || r))
                
checkTest1_9 = and [test1,  test2a, test2b, test3a, test3b, 
                    test4a, test4b, test4c, test5a, test5b,
                    test6a, test6b, test7a, test7b, test8a,
                    test8b, test9a, test9b]   -- True                    

-- Theorem 2.12 
--      Reasoning with propositions that are ALWAYS True or
--      those that are always False.
--
-- dominance laws
test10a = (not True)  <=> False
test10b = (not False) <=>  True

test11a = lequiv (\ p -> p ==> False)  (\ p -> not p)

test13a = lequiv (\ p -> p || True)  (const True)
test13b = lequiv (\ p -> p && False) (const False)

-- identity laws
test14a = lequiv (\ p -> p || False) id
test14b = lequiv (\ p -> p && True)  id

-- law of the excluded middle
test15 = lequiv (\ p -> p || (not p)) (const True)

-- contradiction
test16 = lequiv (\ p -> p && (not p)) (const False)

checkTest11_16 = and [test10a, test10b, test11a, test13a, test13b,
                      test14a, test14b, test15,  test16]  -- True
                      



                                                    


