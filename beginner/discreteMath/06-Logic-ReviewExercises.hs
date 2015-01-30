-- Chapter 6 - Propositional Logic - Review Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

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

    Data Types for Proofs:
    
        data Proof
            = Assume Prop
            | AndI (Proof,Proof) Prop
            | AndEL Proof Prop
            | AndER Proof Prop
            | OrI1 Proof Prop
            | OrI2 Proof Prop
            | OrE (Proof,Proof,Proof) Prop
            | NotE (Proof,Proof) Prop
            | ImpI Proof Prop
            | ImpE (Proof,Proof) Prop
            deriving (Eq,Show)            
            
-}
import Stdm     -- need to run under Hugs/Haskell 98

{-
    Exercise 34
        Prove:  A /\ (not A) |- False
        
    Natural deduction style:
    
        A /\ (not A)            A /\ (not A)
        ------------{/\ EL}      ------------{/\ ER}
             A                     (not A)
        ------------------------------------{ ->E}
                      False
-}
th34 = Theorem [(And A (Not A))] (FALSE)

proof_th34 = ImpE (AndEL (Assume(And A (Not A))) A,
                   AndER (Assume(And A (Not A))) (Not A))
                  (FALSE)
                  
ex34 = check_proof th34 proof_th34

{-
    Exercise 35
        Prove:  A |- not (not A)
        
    Natural deduction style:
    
        A     A -> False
        ----------------{ -> E}
              False
        ----------------------{ -> I}
        (A -> False) -> False
        
-}
th35 = Theorem [A] (Imp (Imp A FALSE) FALSE)
                  
proof_th35 = ImpI (ImpE (Assume A, Assume (Imp A FALSE)) (FALSE))
                  (Imp (Imp A FALSE) FALSE)              
             
ex35 = check_proof th35 proof_th35
             
{-
    Exercise 36
        Prove:  A, A->B, B->C, C->D |- D
        
    Natural deduction style:
        A  A->B
        -------{ -> E}
            B            B->C
            -----------------{ -> E}
                     C                 C->D
                     ----------------------{ -> E}
                                D
-}
th36 = Theorem [A, (Imp A B), (Imp B C), (Imp C D)] (D)

proof_th36 = (((Assume A, Assume(Imp A B))
               {-------------------------} `ImpE`
                    B,  Assume(Imp B C))
                   {-------------------} `ImpE`
                       C, Assume(Imp C D))
                      {----------------} `ImpE`
                              D

ex36 = check_proof th36 proof_th36
                              
                              

