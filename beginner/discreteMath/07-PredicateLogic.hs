-- Chapter 7 - Predicate Logic
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm

{-
    Predicate Logic is an extension of Propositional Logic, it
    adds two 'quantifiers'; it includes everything that is in
    propositional logic; all the definitions, theorems, inference
    rules, algebraic laws, etc. still hold.
    
-}
{-
    7.1 Language of Predicate Logic
    
    Predicate
        A predicate is a statement that an object, x, has a certain
        property. The statement may be True or False depending on 
        the value of 'x'.
        
        Predicates are written as F(x) or G(x,y); it is essentially
        a function that returns a Boolean result.
        
        The 'universe of discourse' is the set of possible values
        that variables of a predicate function can have.
        
        Common Notation for Predicate Logic:
            U       universe of discourse
            c,p     to indicate a constant or particular value
            x,y,z   variables
            F,G,H   predicates
            f(x)    any predicate 'f' applied to 'x'
            
    Quantifiers
        Universal Quantifier  (symbol is an upside down A)
            indicates a predicate holds for 'all' or 'every' member of 
            the universe. Often used to state 'required' properties.
            Universal quantification over an empty set is 'vacuously
            True'
            
        Existential Quantifier (symbol is a reversed capital E)
            indicates 'something' or 'some' x in the universe has a 
            given property. Used to state properties that MUST occur
            at least once. Existential quantification over an empty
            set is 'vacuously False'.
            
        Quantifiers can be restricted to specific 'sets' which must
        be part of (subsets of) the original universe.
        
    Expanding Quantifiers
        If the universe is finite, or if the variables are restricted
        to a finite set, quantifiers may be interpreted as ordinary
        terms in propositional logic and expanded as follows:
        
            Assume U = {c1,c2,c3,...,cn}
            then,
            
                all x. F(x) = F(c1) && F(c2) && ... && F(cn)
               some x. F(x) = F(c2) || F(c2) || ... || F(cn)
               
        i.e. in a finite universe the quantifiers are syntactic
             abbreviations

        EVERY well formed formula (WFF) has a finite size; however,
        there is no bound on how large the formula may be.
        
    Example Expansion:
        Let S = {0,2,4,6} and R = {0,1,2,3}.
        Then we can say that 'every' element in S is twice
        'some' element in R.
        
        for all x in S. there exists some y in R. such that x = 2 x y
        
        Expand the statement starting with the outer quantifier
                (y in R. 0 = 2 x y}
             && (y in R. 2 = 2 x y)
             && (y in R. 4 = 2 x y)
             && (y in R. 6 = 2 x y)
             
        Then expand the inner quantifier
                ((0=2x0) || (0=2x1) || (0=2x2) || (0=2x3))
             && ((2=2x0) || (2=2x1) || (2=2x2) || (2=2x3))
             && ((4=2x0) || (4=2x1) || (4=2x2) || (4=2x3))
             && ((6=2x0) || (6=2x1) || (6=2x2) || (6=2x3))
             
        Note that the 'universe' is the 'union' of S and R
        i.e. {0,1,2,3,4,6} and that 'x = 2 x y' could have
        been written as: F(x,y)
                 
-}
{-
    7.1.4 Scope of Variable Bindings
    
        Quantifiers 'bind' variables by assigning them values
        from a universe.
        The extent of a variable binding is called its 'scope'
        e.g. the scope of 'x' in (exists x).F(x) is the 
             subexpression F(x)
             
             for (all x)S.(exists y)R.F(x,y) the scope of 'x' is
                (exists y)R.F(x,y)
             while the scope of of 'y' is: F(x,y)
             
        A quantifier extends over the smallest expression possible
        unless parentheses indicate otherwise. For example, in
        the expression
            (all x).p(x) || q(x)
            
        the 'x' in q(x) is NOT BOUND by (all x) so it must be
        bound at some 'outer' level (i.e. the expression must
        be embedded in a larger one)
             
-}
{-
    7.1.5 Translating between English and Logic
    
    Often not a direct one to one translation; the difficulty
    is usually in fully understanding what is 'meant'.
    Example, 'All people are not rich.' says
        
        (all x).not R(x)  where R(x) means 'x is rich'
        
    which reads as 'all' as in 'every person' is not rich when
    what is really meant is that 'not all' people are rich
    
        not(all x).R(x)
        
    or, more likely, 'some people are not rich'
    
        (exist x).not R(x)
        
    Example:
        Some birds can fly.
        
        Let the universe be 'all birds'
        Let B(x) mean 'x is a bird'
        Let F(x) mean 'x can fly'
        
            (exists x).B(x) && F(x)
            
        i.e. there exists some members of the universe that
             are birds who can also fly 
             
        Beware of using implication (->) in these situations.
        If a 'frog' somehow got into the universe by the result
        of B(frog) -> F(frog) would be False -> False = True!
                
-}
{-
    7.2 Computing with Quantifiers
    
        Haskell has a number of functions that are useful in 
        computing with predicates logic. (Examples all assume
        the universe is a set of numbers represented as a list.)
        
            forall :: [Int] -> (Int -> Bool) -> Bool
            exists :: [Int] -> (Int -> Bool) -> Bool
            
        'forall' and 'exists' can be nested; it's convenient to
        represent inner quantified expressions as separate functions.
        
        Example 7:
            (all x) in {1,2}.((exists y) in {1,2}.x = y)
            
            The inner assertion can be represented by the 
            function 'inner_fun' the result of which can be
            evaluated using 'forall'
            
        Walkthrough:
              forall [1,2] inner_fun
            = and [inner_fun 1, inner_fun 2]
            = and [exists [1,2] (== 1),
                   exists [1,2] (== 2)]
            = and [or [1==1, 2==1],
                   or [1==2, 2==2]]
            = and [True, True]
            = True        

-}
inner_fun :: Int -> Bool
inner_fun x = exists [1,2] (== x)

ex7 = forall [1,2] inner_fun

{-
    Example 8
        inner_fun is redefined as inner_fun' (see below)
    
    Walkthrough of the code shown below:
          exists [1,2,3] inner_fun
        = or [inner_fun' 1, inner_fun' 2, inner_fun' 3]
        = or [exists [1,2,3] (== 1+2),
              exists [1,2,3] (== 2+2),
              exists [1,2,3] (== 3+2)]
        = or [or [1 == 1+2, 2 == 1+2, 3 == 1+2],
              or [1 == 2+2, 2 == 2+2, 3 == 2+2],
              or [1 == 3+2, 2 == 3+2, 3 == 3+2]]
        = or [or [False, False, True],
              or [False, False, False],
              or [False, False, False]]
        = or [True, False, False]
        = True    
-}
inner_fun' :: Int -> Bool
inner_fun' x = exists [1,2,3] (== x+2)

ex8 = exists [1,2,3] inner_fun

{-
    7.3 Logical Inference with Predicates
    
    Four new rules, Introduction and Elimination rules for both 
    quantifiers, are required to extend propositional logic inference 
    rules to be used with predicate logic.
    
        F(x)   {x arbitrary}                (all x).F(x)
        --------------------{all I}         ------------{all E}
            (all x).F(x)                        F(p)
        
        
            F(p)
        ----------------{exists I}
        (exists x).F(x)
        
         
        (exists x).F(x)    F(x) |- A {x not free in A)
        ----------------------------------------------{exists E}
                               A
                               
    We always assume the universe is non-empty.
    
    Universal Introduction
    ----------------------
        A standard proof technique is to state and prove a property
        about an arbitrary element of a universe and then claim
        the property holds for all members of the universe. 
        
        The use of 'arbitrary' values usually implies 'forall'
        
    Example:
        A simple theorem about lists in Haskell says there are
        two ways of adding a single element to the beginning of
        a list:
        
            Let x :: a  and xs :: [a], then x:xs = [x] ++ xs
            
        This could also be written as:
        
            (all x)::a. (all xs)::[a]. Then x:xs = [x] ++ xs
            
        (++) is defined as:
        
                (++) :: [a] -> [a] -> [a]
                [] ++ ys     = ys                       (1)
                (x:xs) ++ ys = x : (xs ++ ys)           (2)
            
        And the proof, which works for either of the above
        notations, is:
        
            [x] ++ xs
         =  (x:[]) ++ xs            {def of notation}
         =  x:([] ++ xs)            (++).2
         =  x:xs                    (++).1
            
        i.e. if we can prove something is true for arbitrary values
             we can infer the same is true for all possible values
             of those variables.
             
        The 'all' introduction rule cannot be used if the proof is
        based on particular values. A 'free' (unbound by a quantifer) 
        variable is NOT arbitrary.
        
    Universal Elimination
    ---------------------
        This rule says that if you established (proved?) (all x).F(X)
        and 'p' is a 'particular' element in the universe of 'x'
        the you can infer F(p).
        
    Existential Introduction
    ------------------------
        The rules says that if f(p) has been established for a
        particular 'p' then you can infer (exists x).f(x).
        
        i.e. if you can prove one member of the universe has a
             property then you can infer there exists at least
             one (arbitrary?) element with that property.
             
    Existential Elimination
    -----------------------
        If we know F(p_i) holds for some 'i' and that A must
        hold if F(x) holds for arbitrary x, then we can infer A.
    
-}
{-
    Algebraic Laws of Predicate Logic
    ---------------------------------
        (all x).f(x) -> f(c)
                f(c) -> (exists x).f(x)
                
        (all x). not f(x) = not (exists x).f(x)
      (existx x).not f(x) = not (all x).f(x)
        
       Provided that 'x' does not occur free in 'q' ('q' does
       not contain 'x'):
       
        ((all x).f(x)) /\ q = (all x).(f(x) /\ q)
        ((all x).f(x)) \/ q = (all x).(f(x) \/ q)
     ((exists x).f(x)) /\ q = (exists x).(f(x) /\ q)
     ((exists x).f(x)) \/ q = (exists x).(f(x) \/ q)
     
    ((all x).f(x)) /\ ((all x).g(x)) =  (all x).(f(x) /\ g(x))
    ((all x).f(x)) \/ ((all x).g(x)) -> (all x).(f(x) \/ f(x))
           (exists x).(f(x) /\ g(x)) -> (existx x).f(x) /\ (exists x).g(x)
    ((exists x).f(x)) \/ ((exists x).g(x)) = (exists x).(f(x) \/ g(x))
    
    
        
        

-}
