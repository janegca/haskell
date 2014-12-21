-- Informatics 1 - Functional Programming 
-- Tutorial 5 - Logic
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/tutorial5.zip

import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

{- Warmup exercises

    First you we will write some functions to act on input of the 
    user-defined type Fruit. Below, you will find the following data 
    declaration:
    
        data Fruit = Apple(String, Bool)
                   | Orange(String, Int)
        
    An expression of type Fruit is either an Apple(String, Bool) or an 
    Orange(String, Int). We use a String to indicate the variety of the 
    apple or orange, a Bool to describe whether an apple has a worm and 
    an Int to count the number of segments in an orange. For example:
    
        Apple("Granny Smith", False) -- a Granny Smith apple with no worm
        Apple("Braeburn", True)      -- a Braeburn apple with a worm
        Orange("Sanguinello", 10)    -- a Sanguinello orange with 10 segments

-}

-- The datatype 'Fruit'
data Fruit = Apple(String, Bool)
           | Orange(String, Int)

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple("Granny Smith", False) -- a Granny Smith apple with no worm
apple' = Apple("Braeburn", True)      -- a Braeburn apple with a worm
orange = Orange("Sanguinello",10)     -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange("Seville",12),
          Apple("Granny Smith",False),
          Apple("Braeburn",True),
          Orange("Sanguinello",10)]

-- This allows us to print out Fruit in the same way we print out a list, 
-- an Int or a Bool.
instance Show Fruit where
  show (Apple(variety, hasWorm))   
    = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange(variety, segments)) 
    = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

{- 1. Write a function 
            isBloodOrange :: Fruit -> Bool 
            
      which returns True for blood oranges and False for apples and other 
      oranges. Blood orange varieties are: Tarocco, Moro and Sanguinello. 
      For example:
            isBloodOrange(Orange("Moro",12)) == True
            isBloodOrange(Apple("Granny Smith", True)) == False
-}
isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange("Moro", _ ))       = True
isBloodOrange (Orange("Tarocco",_))      = True
isBloodOrange (Orange("Sanguinello", _)) = True
isBloodOrange _                          = False

{- 2. Write a function 
            bloodOrangeSegments :: [Fruit] -> Int 
            
      which returns the total number of blood orange segments in a list of
      fruit.
-}
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments fs = sum [segments f | f <- fs, isBloodOrange f]
    where
        segments (Orange(_, n)) = n
        segments (Apple(_,_))   = 0
          

{- 3. Write a function 
        worms :: [Fruit] -> Int 
        
      which returns the number of apples that contain worms.
-}
worms :: [Fruit] -> Int
worms fs = sum [ 1 | f <- fs, isWormy f]
    where        
        isWormy (Apple(_,hasWorm)) = hasWorm
        isWormy (Orange(_,_))      = False

-- Implementing propositional logic in Haskell
{- The datatype 'Prop' 
        is a representation of propositional formulas
        propositional variables such as P and Q can be represented
        as Var "P" and Var "Q"

-}

type Name = String
data Prop = Var Name            -- propositional variable P, Q, etc.    
          | F                   -- False
          | T                   -- True
          | Not Prop            -- logical not
          | Prop :|: Prop       -- logical Or
          | Prop :&: Prop       -- logical And
          | Prop :->: Prop      -- implication
          | Prop :<->: Prop     -- iff
          deriving (Eq, Ord)

-- list of propositional variable names
type Names = [Name]

-- 'environment' in which to evaluate a proposition
-- contains name-value pairs for the propositional variables
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  not (eval e p) || eval e q
eval e (p :<->: q)    =  eval e p == eval e q

-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
-- i.e. whether there is some assignment of truth values to the variables
--      in the formula that will make the whole formula true
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

{- 4.
    Write the following formulas as Props (call them p1, p2 and p3).
    Then use satisfiable to check their satisfiability and table to print 
    their truth tables.
    
        (a) ((P | Q) & (P & Q))
        (b) ((P | Q) & ((not P) & (not Q)))
        (c) ((P & (Q | R)) & (((not P) |(not Q)) & ((not P) | (not R))))

-}
p1 = ((Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q"))
p2 = ((Var "P" :|: Var "Q") :&: ( Not (Var "P") :|: Not(Var "Q")))
p3 = ((Var "P" :&: (Var "Q" :|: Var "R"))
     :&: (( Not (Var "P") :|: Not (Var "Q")) 
     :&: (Not (Var "P") :|: Not (Var "R"))))

{- 5. 
   (a)A proposition is a tautology if it is always true, i.e. in every 
      possible environment. Using names, envs and eval, write a function 
            tautology :: Prop -> Bool which checks
      
      whether the given proposition is a tautology. Test it on the 
      examples from Exercise (4) and on their negations.
-}
tautology :: Prop -> Bool
tautology p = foldr (&&) True $ map (`eval` p) (envs (names p))

{-
    (b) Create two QuickCheck tests to verify that tautology is working 
        correctly. Use the following facts as the basis for your test 
        properties:
        
        For any property P,
        i.  either P is a tautology, or not P is satisfiable,
        ii. either P is not satisfiable, or not P is not a tautology.
        
        Note: be careful to distinguish the negation for Bools (not) from 
              that for Props (Not).

-}
prop_taut1 :: Prop -> Bool
prop_taut1 p = tautology p || satisfiable (Not p)

prop_taut2 :: Prop -> Bool
prop_taut2 p = not (satisfiable p) || not(tautology (Not p))

{- 6.
    (a) Find the declaration of the data type Prop in tutorial5.hs and 
        extend it with the infix constructors :->: and :<->:.
        
    (b) Find the printer (showProp), evaluator (eval), and name-extractor 
       (names) functions and extend their definitions to cover the new 
       constructors :->: and :<->:. Test your definitions by printing out
       the truth tables:
       
       table (Var "P" :->: Var "Q")         table (Var "P" :<->: Var "Q")
        P Q | (P->Q)                        P Q | (P<->Q)
        - - | ------                        - - | -------
        F F |   T                           F F |   T
        F T |   T                           F T |   F
        T F |   F                           T F |   F
        T T |   T                           T T |   T
        
    (c) Define the following formulas as Props (call them p4, p5, and p6).
        Check their satisfiability and print their truth tables.
        
        i.   ((P -> Q) & (P <-> Q))
        ii.  ((P -> Q) & (P & (not Q)))
        iii. ((P <-> Q) & ((P & (not Q)) | ((not P) & Q)))
        
    (d) Below the 'exercises' section, in the section called 'for 
        QuickCheck', you can find a declaration that starts with:
            instance Arbitrary Prop where
            
        This tells QuickCheck how to generate arbitrary Props to conduct 
        its tests. To make QuickCheck use the new constructors, uncomment 
        the two lines in the middle of the definition:
        
            -- , liftM2 (:->:) subform subform
            -- , liftM2 (:<->:) subform' subform'

        Now try your test properties from Exercise (5b) again.
-}
p4 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")
p5 = (Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))
p6 = (Var "P" :<->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))
                             :|: (Not (Var "P") :&: Var "Q")

{- 7.
    Two formulas are equivalent if they always have the same truth values, 
    regardless of the values of their propositional variables. In other 
    words, formulas are equivalent if in any given environment they are 
    either both true or both false.
    
    (a) Write a function 
            equivalent :: Prop -> Prop -> Bool 
        
        that returns True just when the two propositions are equivalent 
        in this sense. For example:
        
            *Main> equivalent (Var "P" :&: Var "Q") 
                              (Not (Not (Var "P") :|: Not (Var "Q")))
            True
            *Main> equivalent (Var "P") (Var "Q")
            False
            *Main> equivalent (Var "R" :|: Not (Var "R")) 
                              (Var "Q" :|: Not (Var "Q"))
            True

        You can use names and envs to generate all relevant environments, 
        and use eval to evaluate the two Props.
-}
pP = Var "P"
pQ = Var "Q"
pR = Var "R"
p7 = pP :&: pQ
p8 = (Not (Not pP :|: Not pQ ))
p9 = pR :|: Not pR
p10 = pQ :|: Not pQ

equivalent :: Prop -> Prop -> Bool
equivalent p1 p2 = foldr (&&) True $ map (`eval` p) (envs (names p))
    where
        p = p1 :<->: p2

{- 
    (b) Write another version of equivalent, this time by combining the
        two arguments into a larger proposition and using tautology or 
        satisfiable to evaluate it.
-}        
equivalent' :: Prop -> Prop -> Bool
equivalent' p q = tautology (p :<->: q)

{-
    (c) Write a QuickCheck test property to verify that the two versions 
        of equivalent are equivalent.
-}
prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent p1 p2 = equivalent p1 p2 == equivalent' p1 p2

{-
    The subformulas of a proposition are defined as follows:
  
        A propositional letter P or a constant t or f has itself as its 
        only subformula.
        
        A proposition of the form (not P) has as subfomulas itself and all
        the subformulas of P.
        
        A proposition of the form P & Q, P | Q, P -> Q, or P <-> Q has as 
        subformulas itself and all the subformulas of P and Q.
        
    The function 
            fullTable :: Prop -> IO () 
    
    already defined, prints out a truth table for a formula, with a column
    for each of its non-trivial subformulas.
-}
{- 8.
    Add a definition for the function 
            subformulas :: Prop -> [Prop] 
    
    that returns all of the subformulas of a formula. For example:
    
        *Main> map showProp (subformulas p2)
        ["((P|Q)&((~P)&(~Q)))","(P|Q)","P","Q","((~P)&(~Q))","(~P)","(~Q)"]

    (We need to use map showProp here in order to convert each proposition 
    into a string; otherwise we could not easily view the results.)
    
    Test out subformulas and fullTable on each of the Props you defined 
    earlier (p1{p6).
-}
subformulas :: Prop -> [Prop]
subformulas (Not p)      = Not p : subformulas p
subformulas (p :|: q)    = (p :|: q)   : nub(subformulas p ++ subformulas q)
subformulas (p :&: q)    = (p :&: q)   : nub(subformulas p ++ subformulas q)
subformulas (p :->: q)   = (p :->: q)  : nub(subformulas p ++ subformulas q)
subformulas (p :<->: q)  = (p :<->: q) : nub(subformulas p ++ subformulas q)
subformulas p            = [p]

-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
