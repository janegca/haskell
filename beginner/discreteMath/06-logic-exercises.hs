-- Chapter 6 - Propositional Logic - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm     -- need to run under Hugs/Haskell 98

{-
    Exercise 2
    
        Check values for a number of proposition statements
-}

ex2a = (False /\ True)                           == False
ex2b = True \/ (not True)                        == True
ex2c = not (False \/ True)                       == False
ex2d = (not (False /\ True)) \/ False            == True
ex2e = (not True) ==> True                       == True
ex2f = True \/ False ==> True                    == True
ex2g = True ==> (True /\ False)                  == False
ex2h = False ==> False                           == True
ex2i = (not False) <=> True                      == True
ex2j = True <=> (False ==> False)                == True
ex2k = False <=> (True /\ False ==> True)        == False
ex2l = (not (True \/ False)) <=> (False /\ True) == True

testEx2 = ex2a && ex2b && ex2c && ex2d && ex2e && ex2e && ex2f
       && ex2g && ex2h && ex2i && ex2j && ex2k && ex2l
       
{-
    Exercise 3
    
    Use the truth table functions to determine which of the following
    formulas are tautologies.
    
    (a) (True && P) || Q
    
            P     Q     True && P    (True && P) || Q
            -----------------------------------------
            True  True    True            True
            True  False   False           False
            False True    False           False
            False False   False           False
            
        Not a tautology
    
    (b) (P || Q) -> (P && Q)
    
            P     Q       P || Q    P && Q   A -> B
            ------------------------------------------
            True  True      True      True     True
            True  False     True      False    False
            False True      True      False    False
            False False     False     False    True
    
        Not a tautology.
    
    (c) (P && Q) -> (P || Q)
    
            P     Q       P && Q    P || Q   A -> B
            -----------------------------------------
            True  True      True     True     True
            True  False     False    True     True
            False True      False    True     True
            False False     False    False    True
            
        Is a tautology.
    
    (d) (P || Q) -> (Q || P)
    
            P     Q       P || Q    Q || P   A -> B
            -----------------------------------------
            True  True      True      True    True
            True  False     True      True    True
            False True      True      True    True
            False False     False     False   True
            
        Is a tautology.
    
    (e) ((P || Q) && (P || R)) <-> (P && (Q || R))
    
                                A      B      C     D     E
            P     Q     R      P||Q  P||R   Q||R   A&&B  P&&C   D<->E
            ------------------------------------------------------------
            True  True  True   True  True   True   True  True   True
            True  True  False  True  True   True   True  True   True
            True  False True   True  True   True   True  True   True
            True  False False  True  True   False  True  False  False
            False True  True   True  True   True   True  False  False
            False True  False  True  False  True   False False  True
            False False True   False True   True   False False  True
            False False False  False False  False  False False  True
         
        Not a tautology.         
-}       
{-
    For the next few exercises, determine:
        (a) which statements are Well Formed Fomulas (WFF's)
        (b) if well formed, classify them as one of:
                tautology       - always true regardless of var values
                contradiction   - always false regardless of var values
                satisfiable     - sometimes true (but not a tautology)
                
    Exercise 4
            (True && P) || Q    
            
            P   Q   T&&P   A||Q
            -------------------         Satisfiable, not a tautology
            T   T     T     T
            T   F     T     T
            F   T     F     T
            F   F     F     F
            
    Exercise 5
           (P || Q) && (P || R) <-> P && (Q || R)

                     A     B     C    D     E
        P   Q   R  P||Q  P||R  Q||R  A&&B  P&&C  D<->E
        ----------------------------------------------
        T   T   T    T     T     T    T     T      T      Satisfiable,
        T   T   F    T     T     T    T     T      T      not a tautology
        T   F   T    T     T     T    T     T      T
        T   F   F    T     T     F    T     F      F
        F   T   T    T     T     T    T     F      F
        F   T   F    T     F     T    F     F      T
        F   F   T    F     T     T    F     F      T
        F   F   F    F     F     F    F     F      T
        
        
    Exercise 6
    
            ((P && not Q) || (Q && not P)) -> not(P <-> Q)
            
                            A     B       C    D
        P   Q   NP   NQ   P&&NQ  Q&&NP  A||B P<->Q  ND  C->ND
        -----------------------------------------------------
        T   T   F    F      F      F      F     T    F    T
        T   F   F    T      T      F      T     F    T    T
        F   T   T    F      F      T      T     F    T    T
        F   F   T    T      F      F      F     T    F    T
        
        Tautology
    
    Exercise 7
    
            (P -> Q) && (P -> (not Q))
            
                      A      B
        P   Q   NQ   P->Q   P->NQ   A&&B
        --------------------------------
        T   T    F    T       F      F         Satisfiable, not a tautology
        T   F    T    F       T      F
        F   T    F    T       T      T
        F   F    T    T       T      T
        
    Exercise 8
    
            (P -> Q) && ((not P) -> Q)
            
        P   Q   NP   P->Q   NP->Q   A&&B
        --------------------------------
        T   T   F     T       T      T      Satisfiable, not a tautology
        T   F   F     F       T      F
        F   T   T     T       T      T
        F   F   T     T       F      F
        
    Exercise 9
    
            (P -> Q) <=> ((not Q) -> (not P))
            
                           A       B
        P   Q   NP   NQ   P->Q   NQ->NP  A<=>B
        --------------------------------------
        T   T   F    F     T       T       T       Tautology
        T   F   F    T     F       F       T
        F   T   T    F     T       T       T
        F   F   T    T     T       T       T
            
-}
-- PROVING LOGICAL INFERENCE --------------------------------------------
{-  
    Exercise 10
    
        Prove P,Q,R |- P /\ (Q /\ R)
        
                      Q   R
                    --------  {/\ I}
                P    Q /\ R 
                ------------  {/\ I}
                P /\ (Q /\ R)
                  
-}
{-
    Exercise 11
    
        Consider the following two propositions:
            x = A /\ (B /\ (C /\ D))
            y = (A /\ B) /\ (C /\ D)
        
        Describe the shapes of the proofs for x and y, Assuming A, B, C, 
        and D. Suppose each proposition has 2^n propositional variables. 
        What then would be the heights of the proof trees?
        
    Shape of the proof for 'x' would be triangular with more inference
    on the right side than the left; height: 2^n
    Shape of the proof for 'y' would be square (symmetrical); height: n
    
-}
{-
    Exercise 12
    
        Prove (P /\ Q) /\ R |- P /\ (Q /\ R)
        
                        (P /\ Q)/\R         
                        ----------- {/\EL}
            (P/\Q)/\R           P /\ Q        (P/\Q)/\R
            ---------{/\EL}     -------{/\ER} --------- {/\ER}
            P /\ Q                Q            R
            ------{/\EL}       ----------------- {/\ I}
               P                   Q /\ R
            ------------------------------- {/\ I}
                    P /\ (Q /\ R)
        
-}
{-
    Exercise 13
    
        Prove P, P -> Q, (P /\ Q) -> (R /\ S) |- S
        
            P        Q
            ----------{/\I}
               P /\ Q           P /\ Q  ->  R /\ S
            ------------------------------------{ ->E}   
                          R /\ S
                          ------ {/\ER}
                             S            
-}
{-
    Exercise 14
    
        Prove P -> Q, R -> S, P /\ R |- S /\ R
        
            P /\ R
            ------{/\ER}
               R     R -> S         P /\ R
               ------------{ ->E}    -------{/\ER}
                     S                 R
                     ---------------------{/\ I}
                           S /\ R
                           
-}
{-
    Exercise 15
        Prove P |- Q -> P /\ Q
        
                    P  Q
                    ----{/\ I}
                Q   P/\Q            'Q' is discharged
                ---------{ -> I}
                Q -> P/\Q

-}
{-
    Exercise 16
        Prove |- P /\ Q -> Q /\ P
        
                P   Q
                ------{/\ I}
                P /\ Q        Q /\ P          
                ---------------------{ -> I}   P and Q are discharged
                    P /\ Q  -> Q /\ P
-}
{-
    Exercise 17
        Prove P -> False \/ P
        
            F \/ P
            ------
               P     False \/ P
               ----------------
               P -> False \/ P

-}
{-
    Exercise 18
        Prove |- P /\ Q -> Q \/ P
        
            P   Q
            ------{/\ I}
            P /\ Q
        ------------------{\/ IL}
        (P/\Q) \/ (Q\/P)

-}
{-
    Exercise 19
        Use the inference rules to calculate the value of True /\ True
        
        True is defined as (False -> False)
        
            (False -> False) /\ (False -> False)
            ------------------------------------{/\ER}
                       (False -> False)
                       
                       
        PROVIDED SOLUTION:
        
        Prove that 'True->True' -> True' and 'True -> True /\ True'
        
                True /\ True
                ------------{/\ER}
                     True
            ----------------------{ ->I}
            (True /\ True) -> True

            
                True    True
                ------------{/\I}
                True /\ True
            --------------------{ ->I}    
            True -> (True /\ True)
-}
{-
    Exercise 20
        Use the inference rules to calculate the value of True \/ False
        
                                 (False /\ False)
                                 ----------------{/\ER} 
                (False->False) \/      False
                ---------------------------------{CTR}
                          (False -> False)
                          
        PROVIDED SOLUTION:
        
        We prove that 'False \/ True -> True' and then that
        'True -> False \/ True'
        
                               True       False
                               ----{ID}   -----{CTR}
            True \/ False     True        True
            ------------------------------------{\/E}
                           True
            ------------------------------------{ ->I}
                     True \/ False -> True
                     
                     
                             True
                         -------------{\/IL}
                         True \/ False
                    ------------------------{ ->I}
                      True -> True \/ False
-}
{-
    Exercise 21
    
    Notice that in the proof of |- True /\ False -> False we used {/\ER}
    to obtain False from (False -> False) /\ False, and everything worked
    fine. However, we could have used {/\EL} instead to infer False ->
    False, which is True. 
    
    What would happen if that choice is made? Would it result in
    calculating the wrong value of True ∧ False? Is it possible to show 
    that True /\ False is not logically equivalent to True?


                    (False -> False) /\ False
                    -------------------------{/\EL}
                         False -> False
                         --------------{ID}
                               False
                               
    [No solution provided for this one; if False implies itself then
     it can only be itself, so ID rule applies?]    
-}
{- PROOF CHECKING USING THE COMPUTER ------------------------------------

    Syntax for using Stdm.lhs
    
    Data type for inference rules
    
        data Proof
        = Assume Prop
        | AndI (Proof,Proof) Prop       -- /\ Introduction
        | AndEL Proof Prop              -- /\ Elimination Left
        | AndER Proof Prop              -- /\ Elimination Right
        | OrI1 Proof Prop               -- \/ Inference Left
        | OrI2 Proof Prop               -- \/ Inference Right
        | OrE (Proof,Proof,Proof) Prop  -- \/ Elimination
        | NotE (Proof,Proof) Prop       -- Negation 
        | ImpI Proof Prop               -- -> Introduction
        | ImpE (Proof,Proof) Prop       -- -> Elimination
        deriving (Eq,Show)

-}

-- Example proofs
proof1 =
    ImpI
        (ImpI
            (AndI
                ((AndER
                    (Assume (And P R))
                    R),
                 Assume Q)
            (And R Q))
         (Imp (And P R) (And R Q)))
        (Imp Q (Imp (And P R) (And R Q)))

chkProof1 = check_proof example_theorem proof1      -- valid

-- introduce an error into proof1
proof2 =
    ImpI
        (ImpI
            (AndI
                (Assume Q,
                    (AndER
                        (Assume (And P R))
                    R))
                (And R Q))
            (Imp (And P R) (And R Q)))
        (Imp Q (Imp (And P R) (And R Q)))
        
chkProof2 = check_proof example_theorem proof2 

{-
    Resulting output:
    
    Main> chkProof2
    *** The proof is NOT valid ***
    Reported errors:
     .AndI: the conclusion (And R Q) 
            is not the logical And of the assumption (Q) 
            with the assumption (R)
-}       

{- Exercise 22

    Suppose we simply replace R & Q below the {AndI} line with
    Q&R. This fixes the Invalid And-Introduction error, but it introduces
    another error into the proof.
    
    (a) Edit proof2 to reflect this change; call the result proof3.
    (b) Decide exactly what is wrong with proof3.
    (c) Run the proof checker on proof3, and see whether it reports the
        same error that you predicted.
-}
proof3 =
    ImpI
        (ImpI
            (AndI
                (Assume Q,
                    (AndER
                        (Assume (And P R))
                    R))
                (And Q R))
            (Imp (And P R) (And R Q)))
        (Imp Q (Imp (And P R) (And R Q)))

ex22 = check_proof example_theorem proof3
        
{-
    Output:
    
    Main> ex22
    *** The proof is NOT valid ***
    Reported errors:
     .ImpI: the conclusion in (Imp (And P R) (And R Q)) 
            doesn't match the conclusion above line (And Q R)
    
-}        
{- - Data type for Propositions

        data Prop
            = FALSE
            | TRUE
            | A | B | C | D | E | G | H | I | J | K | L | M
            | N | O | P | Q | R | S | U | V | W | X | Y | Z
            | Pvar String
            | And  Prop Prop
            | Or   Prop Prop
            | Not  Prop
            | Imp  Prop Prop
            | Equ  Prop Prop
            deriving (Eq,Show)

-}
{-
    Exercise 23. 
    
    Define each of the following well-formed formulas as a Haskell
    value of type Prop.

    (a) P
    (b) Q \/ False
    (c) Q -> (P -> (P /\ Q))
    (d) P /\ (not Q)
    (e) not P -> Q
    (f) (P /\ not Q) \/ (not P /\ Q) -> (P \/ Q)

-}
ex23a = P
ex23b = Or Q FALSE
ex23c = Imp Q (Imp P (And P Q))
ex23d = And P (Not Q)
ex23e = Imp (Not P) Q
ex23f = Imp (Or (And P (Not Q)) (And (Not P) Q)) (Or P Q)

{-
    Exercise 24.
    
    Translate each of the following Haskell expressions into the 
    conventional mathematical notation.
    
    (a) And P Q                           P /\ Q
    (b) Imply (Not P) (Or R S)            (not P) -> R \/ S  
    (c) Equ (Imply P Q) (Or (Not P) Q)    (P -> Q) == (not P \/ Q)

-}
{--
    Exercise 25
    
        Simplify (P /\ False) \/ (Q /\ True)
        
            (P /\ False) \/ (Q /\ True}
          = False \/ (Q /\ True)            {/\ null}
          = False \/ Q                      {/\ identity}
          = Q \/ False                      {commutativity}*
          = Q                               {\/ identity}
          
          
        * missed this step in my original answeer; guess you have to put
          everything in the laws original form

--}
{--
    Exercise 26
    
        Prove the equation (P /\ False) /\ True = False
        
        (P /\ False) /\ True 
      - False /\ True               {/\ null}
      = False                       {/\ operation}
        
        [no provided solution]
--}
{--
    Exercise 27
    
        Prove (P /\ ((Q \/ R) \/ Q)) /\ S = S /\ ((R \/ Q) /\ P)
        
            (P /\ ((Q \/ R) \/ Q)) /\ S
          = S /\ (P /\ ((Q \/ R) \/ Q))         {/\ commutative}
          = S /\ (((Q \/ R) \/ Q) /\ P)         {/\ commutative}*
          = S /\ ((Q \/ (R \/ Q)) /\ P)         {\/ associative}*
          = S /\ ((Q \/ (Q \/ R)) /\ P)         {\/ commutative}*
          = S /\ ((Q \/ Q) \/ R)) /\ P)         {\/ associative|*
          = S /\ ((Q \/ R) /\ P)                {\/ idempotent}
          = S /\ ((R \/ Q) /\ P)                {\/ commutative}
       
        * originally went from (Q \/ (R \/ Q)) to
         (Q \/ R \/ Q) to (Q \/ Q \/ R) to (Q \/ R) using associative law
         to drop internal parentheses and commutative law to swap 
         positions
--}
{--
    Exercise 28
    
        Prove P /\ (Q /\ (R /\ S)) = ((P /\ Q) /\ R) /\ S
        
            P /\ (Q /\ (R /\ S))
          = P /\ (Q /\ R /\ S)              {/\ associative}
          = P /\ Q /\ R /\ S                {/\ associative}
          = (P /\ Q) /\ R /\ S              {/\ associative}
          = ((P /\ Q) /\ R ) /\ S           {/\ associative}
          
    [no provided solution]
--}