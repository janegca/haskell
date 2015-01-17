-- Chapter 6 - Propositional Logic
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm     -- need to run under Hugs Haskell 98

{-

    A 'proposition' is a value that is either True or False and stands
    for an English statement that could be either true or false; the
    CRUCIAL difference is that a proposition MUST be either True or 
    False; no shades of grey.
    
    Examples of proposition variables and the sentences they stand for:
    
            A = The sun is shining.
            B = I feel happy.
            C = Cats are furry.
            D = Elephants are heavy.
    
    Next convert English sentences into a mathematical statement using
    the propositional variables:
    
        The sun is shining and I feel happy.    => A and B
        Cats are furry and elephants are heavy. => C and D
        
    English sentences that rely on context for their meaning cannot
    be reduced to propositional variables i.e. 
    
-}
-- 6.2 Logical Operators -------------------------------------------------
{-
   /\ And (&&)  [logical conjunction]
    - used to claim BOTH statements are True
    - A && B  is the statement that A 'and also' B are True
    - it does not, of itself, prove that both A and B are True,
        they could also both be false or one true and the other false
    - nor does the statement establish a connection between A and B
    - need to establish the truth of both statements individually before
      ascertaining whether the statement 'A /\ B' is True or False
    - a truth table (covering all possibilities) of A And B is:
 
              A     B    A \/ B
            False False   False
            False True    False
            True  False   False
            True  True    True
            
    \/ Or (||) Inclusive logical Or [logical disjunction]
    
    - A Or B is a statement that either A or B is True or possibly
      both are True
    - again, it is simply a statement, not proof that one of A or B
      IS true; neither does it establish a connection between A or B
    - a truth table (covering all possibilities) of A Or B is:

              A      B    A \/ B
            False  False   False
            False  True    True
            True   False   True
            True   True    True
            
    Xor Exclusive Logical Or 

    - either A or B is true BUT not both
    - 'exclusive' as, if one statement is true, the possibility of the
      other being true is 'excluded'

              A      B    A Xor B
            False  False   False
            False  True    True
            True   False   True
            True   True    False        -- compare to A \/ B table
    
    - unless specifically indicated, 'or' is usually taken to mean
      inclusive or
      
    Logical 'not' [logical negation]
    - negates a logical statement (returns its opposite)
    
              A     not A
            False   True
            True    False
    
    (->) Logical Implication
    
    - conditional statement: 
            A -> B read as 'If A is True, then B is True'
    
              A      B     A -> B
            False  False    True
            False  True     True
            True   False    False
            True   True     True
            
    - [confusing table
         if A is False, then B is False   - is a true statement, A == B
         if A is False, then B is True    - B can be true even if A is false
         if A is True,  then B is False   - B can be false even if A is true
         if A is True, then B is True     - is a true statement, A == B
         
         most languages 'short circuit' logical operations so if they
         see A is False they won't even bother to look a B never mind
         execute it; the truth table captures this 'indifference' to B
         
        i.e. B will only be executed if A is True so it is only the
             last 2 cases we need to be concerned with
      ]
      
    -  'Logical implications say NOTHING about cause and effect'
    
    (<->) (==) Logical Equivalence  (A -> B) /\ (B -> A) (A == B)
        
    - used to claim that BOTH A and B have the SAME value
      i.e. both are True or both are False

              A      B     A <-> B
            False  False     True
            False  True      False
            True   False     False
            True   True      True
    
    - note that 'logical equivalence is not strictly the same as 
      the ordinary idea of equality [details to come]
      
    NOTE:
        Truth tables often are written out using abbreviations either
        'T' for true and 'F' for false or, '1' for true and '0' for
        false. (in imperative languages without a Boolean type, 1 and
        and zero are normally used to represent True or False, or 
        any non-zero value for True, and 0 for false).
   
-}
-- 6.3 Language of Propositional Logic -----------------------------------
{-
    A proposition that makes sense is called a 'well-formed formula' WFF
    or 'Woof'.  A WFF has a 'well-defined meaning' which makes it
    worthwhile to work out (using truth tables) the conditions under
    which it would be true.
    
    For a computer language, a proposition, when expressed in Boolean
    values, is 'well-formed' if it is syntactically correct.
    
    Rules for valid WFF's:
        the constants True and False
        any propositional variable P, Q, R, etc
        if 'a' is a WFF then (not a) is a WFF
        if 'a' and 'b' are WFF's then the following are valid WFF's:
            (a && b)
            (a || b) 
            (a -> b) 
            (a <-> b) 
            
    The rules may be used to build nested formulas. For example,
    
            (P -> (Q /\ R))
-}            
-- 6.3.2 Logical Operator Precedence -------------------------------------
{-
   Logical operators have the following precedence (highest to lowest):
                        
                       Implemented in Stdm.lhs as:
        not               not (haskell operator)  
        and /\ (&&)       /\  (or use haskell operator)
        or  \/ (||)       \/  (or use haskell operator)
        ->                =>   - no haskell implementation
        <->               <=>  - no haskell implementation
-}
-- 6.4.1 Truth Table Calculations and Proofs -----------------------------
{-
    Many propositions can be either True or False; however, there are
    some where this is not the case; special names exist for these
    exceptions:
    
    tautology   - a proposition that is ALWAYS TRUE, regardless of
                  the value of its variables
                  eg P or (not P) is always True
                  
    contradiction - a proposition that is ALWAYS FALSE, regradless
                    of the values of its variables
                    eg P and (not P) is alway False
                    
            P   (not P)    P \/ not P   P /\ not P
        --------------------------------------------
         True   False         True         False
         False  True          True         False
         
    There is a meta-operator, double-turnstile ( |= ), which implies
    that if all the propositions to its left are true, then
    the proposition to its right is also true.
    
                     P1, P2, P3 ... |= Q
                    
    i.e. the 'double-turnstile' operator is concerned about the
         'meaning' of each proposition (its 'semantics')

    A 'model of the logical system' consists of the truth values,
    True and False, and a method for calculating the meaning of
    any WFF.

    The statement |= Q means Q is always true, regardless of the
    values of its propositions (it's a tautology)    

-}
-- 6.4.2 Limitations of Truth Tables -------------------------------------
{-
    Truth tables are straight-forward and reasonably easy to crank
    out; however, they give you no insight into 'why' something is
    True or False/sometimes True, sometimes False.
    
    The size of a truth table grows at the rate of 2^k where k is the
    number of propositional variables so they can become quite large
    for interesting problems.

-}
-- 6.5 Natural Deduction: Inference Reasoning ----------------------------
{-
    'Logical inference' means to decide, beyond a shadow of doubt,
     whether or not a set of statements are true.
     
    In order to do this we need to:
    
    - 'define' the set of statements using an 'object language', 
      (typically as propositional expressions)
      
    - establish 'inference rules' (methods for inferring new facts from
      the given information)
      
    - define the form of the argument such that every step in the 
      reasoning is justified by an inference rule
     
    Standard notation for formal proofs:
        From 'propositional logic'
        
        P1, P2, P3, ... |= Q
            if all the propositions are true, then Q is true
            
        P |= Q -> P
            if P is thre then Q -> P is true
    
        From Natural Dedcution:
        
        P1, P2, P3, ... |- Q  
        
            If all the propositions are known, then Q can be inferred
            formally my using the inference rules, expressed as a
            'sequent'
            
        P |- Q -> P
        
            There is a proof of Q -> P and the proof assumes P
            
        |=  double-turnstile, concerned with the ultimate truth of
            the statements
                
        |-  turnstile, concerned with whether we have a proof
                
    Every step of reasoning must be backed up by an inference rule.
    A generic (intuitive) inference rule is:
        If statement one and statement two are established (assumed or 
        proven) then you may infer statement three.
        
        where,
            statement one and two are called 'assumptions'
            statement three is called the 'conclusion'
            
        and,
            there may be multiple assumptions (not limited to two)
            
    Example,
        If you know proposition 'a' and proposition 'b' then you
        may infer 'a and b'
        
        where,
            'a', 'b'  are meta-variables that can be any WFF
            
    Meta-variables belong to the meta-language are written in lowercase 
    letters. The value of a meta-variable is a WFF
            
    Propositional variables belong to the object language and are written
    in uppercase letters. The value of a propositional variable is either
    True or False.
        
    Inference rules are written as:
            Statement 1   Statement 2           a    b
            -------------------------         ----------- 
                   Statement 3                  a /\ b
                   
    If Statement 1 and 2 are true then Statement 3 is guaranteed to be true
    The reverse is NOT true, you can not infer 'a' from 'a /\ b'
    
-}
-- INFERENCE RULES OF PROPOSITIONAL LOGIC -------------------------------
{-
    Broken into two categories:
    
        Introduction rules
            have a conclusion into which a new new logical operator
            has been introduced; used to build up complex expressions
            
        Elimination rules
            require an assumption that uses a logical operator which
            is eliminated from the conclusion
            
    Basic Operators (primitives):
        there are only three: /\, \/, ->
        and one object, False
        
        everything else is an abbreviation [syntactic sugar], being
        defined in terms of the basic operators and False
        
            True    = False -> False
            not a   = a -> False
            a<->b   = (a -> b) /\ (b -> a)
        
        Example
            not False = False -> False = True
            not True  = True -> False = (False -> False) -> False
            
    Inference proofs have a natural tree structure, with the assumptions
    forming the leaves and the subproofs, the nodes (forks)
-}
{- SUMMARY OF INFERENCE RULES --------------------------------------------

    And    a    b         a /\ b           a /\ b
           ------{/\I}    ------{/\EL}     ------{/\ER}
           a /\ b            a                b
           
    Or       a               b             a \/ b   a |- c  b |- c
           ------{\/IL}   ------{\/IR}     ------------------------{\/E}
           a \/ b         a \/ b                       c
           
    Imp     a |- b          a   a -> b
           -------{->I}     ----------{->E}
            a -> b                b
            
            
            a            False             not a |- False
           ---{ID}       -----{CTR}        --------------{RAA}
            a              a                      a


-}

{-  THE INFERENCE RULES --------------------------------------------------

    {/\ I} And Introduction Rule
    ----------------------------
    
                      a   b
                    --------- {/\ I}
                      a /\ b
                      
        If you know some proposition 'a' is true and another proposition, 
        'b', is true then you may infer that 'a and b' are true.
        
    {/\ EL, ER} And Elimination Rule (Left and Right)
    -------------------------------------------------
            
                a /\ b             a /\ b
                ------ {/\EL}      ------ {/\ ER}
                   a                  b
                  
        If 'a and b' is known to be true, then 'a' and 'b' are both must
        be true. The 'And Elimination Left' rule retains the argument
        on the left; the 'And Elimination Right' rule retains the 
        argument on the right.
        
    {-> E} Imply Elimination (Modus Ponens)
    ---------------------------------------
    
                a     a -> b
                ------------ {-> E}
                      b
                      
        If you know that 'a' is true and that 'a implies b' then you can
        infer 'b'.
                 
        When we have chains of implications i.e. a->b, b->c, etc then we
        can infer c        
        
    {-> I} Imply Introduction
    -------------------------
    
                a |- b
                ------{-> I}
                a -> b
                
        To infer a->b you must have proof of 'b' using 'a' as an
        assumption.
        
        
        The 'Implication Chain Rule'
        ----------------------------
                    a -> b, b -> c |- a -> c
                    
            Proof:
            
                    a  a -> b                'a' is 'discharged'
                    ---------{-> E}           it only exists temporarily
                         b       b -> c       to allow the proof of 'b'
                         --------------{-> E} it is not required on the
                                c             'left' of the |- statement
                            ---------{-> I}   but only on the 'right'
                              a -> c
                              
            [Note: a->b and b->c  DO NOT appear in the conclusion
                   so they are NOT discharged; they are required
                   assumptions and must appear on the left.]
                   
        'Modus Tollens' (A corollary to the implication chain rule)
        -----------------------------------------------------------
        
                a -> b, (not b) |- (not a)
                
            Proof:
                Expand the abbreviations, so (not a) becomes a -> False
                and (not b) becomes b -> False
                Which produces the sequent
                
                    a->b, b->False |- a -> False
                    
                which is simply the sequent proved in the Implication
                Chain Rule.
             
        {\/IL IR} Or Introduction Left and Right
        ----------------------------------------
            
                    a                       b
                 --------{\/IL}       ------------ {\/ IR}
                  a \/ b                a \/ b
                  
            If 'a' is true or 'b' is true then you can infer 'a \/ b'
            and 'b \/ a' are both true.
            
        {\/E} Or Elimination  (Proof by Case Analysis)
        ----------------------------------------------
        
                a \/ b    a |- c     b |- c
                --------------------------- (\/E)
                              c
                              
        You can infer nothing directly for 'a \/ b' even if it is
        true as either a or b might be false; however, if you know
        'a \/ b' and some conclusion 'c' that can be inferred from
        'a' and also inferred from 'b', then 'c' must be true.
        
        Formally, Or Elimination amounts to the following argument:
            There are two cases
                (1) if 'a' is true, then 'c' holds
                (2) if 'b' is true, then 'c' holds
                therefore, 'c' is true
                
        {ID} Identity
        -------------
                                a
                               --- {ID}
                                a
                                
        If you know 'a' is true, then 'a' is true
        
        {CTR} Contradiction
        -------------------
                                False
                                ----- {CTR}
                                   a
                                   
        You can 'assume anything at all' given the assumption that
        False 'is' True. i.e. the assumption is that False is 'untrue'
        
        {RAA} Reductio Ad Absurdum
        --------------------------
                                not a |- False
                                --------------{RAA}
                                      a
        
        The 'reduce to absurdity' rule says that if you can infer
        false from 'not a' then 'a' must be true.
        
        
    THE INFERENCE RULES "PROVIDE A COMPLETE FOUNDATION FOR PROPOSITIONAL
    LOGIC" i.e. all truth table values can be proven by using the 
    inference rules.
        
-}


