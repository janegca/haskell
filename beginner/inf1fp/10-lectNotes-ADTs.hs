-- Informatics 1 - Functional Programming 
-- Lecture 10 - Algebraic Data Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect10.pdf
--  Video: 10/27/2014
--    http://groups.inf.ed.ac.uk/vision/VIDEO/2014/inf1fp.htm

{-
    Expression Trees
    
    Representing data in tree structures
    
    Example of simple functions over data types.
    
-}
import Data.List (nub)

-- data type for various arithmetic Expressions
data Exp = Lit Int          -- integer literal as an Exp
         | Add Exp Exp      -- sum of two expressions
         | Mul Exp Exp      -- product of two expressions
         
evalExp :: Exp -> Int     -- definitions for Exp types on evaluation
evalExp (Lit n)   = n                      -- definition for Lit
evalExp (Add e f) = evalExp e + evalExp f  -- definition for Add
evalExp (Mul e f) = evalExp e * evalExp f  -- definition for Mult

showExp :: Exp -> String  -- definitions for Exp types on display
showExp (Lit n) = show n
showExp (Add e f) = par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f) = par (showExp e ++ "*" ++ showExp f)

par :: String -> String     -- pretty parsing of an Exp type
par s = "(" ++ s ++ ")"

-- examples of creating an expression for evaluation
e0, e1 :: Exp
e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)

{-
    Order of evaluation follows tree form with the parentheses
    providing the tree 'structure':
    
    e0                          e1
            Add                       Mul
           /   \                     /   \
          Lit  Mul                  Add   Lit
           |  /   \                /   \   |
           2 Lit  Lit             Lit  Lit 3
              |    |               |    |
              3    3               2    3
              
    So for e0, 
        Add takes two Exps as arguments: Lit and Mul
        Lit takes an integer as an argument: 2
        Mul takes two Exps as arguments: Lit and Lit
        The two Lits evaluate to Integers 2 and 3
        
    When e0 is passed as an argument to evalExp, Add
    and Mul are replaced with the appropriate arithmetic
    operators or values, giving:
            
              Add                    +     
             /   \                 /   \    
            Lit  Mul              2     *  
             |  /   \                 /   \ 
             2 Lit  Lit              3     3
                |    |
                3    3           
           
    Output:
    
        *Main> showExp e0
        "(2+(3*3))"
        *Main> showExp e1
        "((2+3)*3)"
    
        *Main> evalExp e0
        11
        *Main> evalExp e1
        15
        
        *Main> showExp e2
        "(2+3)"
        *Main> evalExp e2
        5
        
-}
{-
    Alternative data definition using Infix pattern
    
    (exactly the same as above definition other than
     use of backticks to create the Exp types as infix
     operators. 
     
     Note that you don't have to define the types this way in 
     order to treat them as 'infix' see e2 and e3 below).
    
        data Exp = Lit Int
                 | Exp ‘Add‘ Exp
                 | Exp ‘Mul‘ Exp

        evalExp :: Exp -> Int
        evalExp (Lit n) = n
        evalExp (e ‘Add‘ f) = evalExp e + evalExp f
        evalExp (e ‘Mul‘ f) = evalExp e * evalExp f
        
        showExp :: Exp -> String
        showExp (Lit n)     = show n
        showExp (e ‘Add‘ f) = par (showExp e ++ "+" ++ showExp f)
        showExp (e ‘Mul‘ f) = par (showExp e ++ "*" ++ showExp f)                 
-}
e2, e3 :: Exp
e2 = Lit 2 `Add` (Lit 3 `Mul` Lit 3)
e3 = (Lit 2 `Add` Lit 3) `Mul` Lit 3

{-
    Example output:
    
        *Main> showExp e2
        "(2+(3*3))"
        *Main> evalExp e2
        11
        *Main> showExp e3
        "((2+3)*3)"
        *Main> evalExp e3
        15       
-}
{-
    The data type (and corresponding definitions) could also have
    been defined using symbols as infix operators (wrapping the
    operators in colons lets the compiler know they are to be
    used as constructors):
    
        data Exp = Lit Int
                 | Exp :+: Exp
                 | Exp :*: Exp    
                 
    Note: we could write Exp :+ Exp or Exp :* Exp
          the second colon has been added to make it more
          obvious
                 
    In which case the examples would be written as:
    
            e0 =  Lit 2 :+: (Lit 3 :*: Lit 3)
            e1 = (Lit 2 :+: Lit 3) :*: Lit 3
-}
{-

    II. PROPOSITIONS
        
        Data type for propositional logic
    
-}
type Name = String          -- type synonym

data Prop = Var Name        -- variable name (i.e. "a", "b", ...)
          | F               -- False
          | T               -- True
          | Not Prop        -- logical negation of Prop
          | Prop :|: Prop   -- logical Or for Prop  (disjunction) (P || Q)
          | Prop :&: Prop   -- logical And for Prop (conjunction) (P && Q)
    deriving (Eq, Ord)      -- provides equality and order relations

type Names = [Name]         -- type synonym

-- an 'Env'(ironment) is an association between variable 
-- names and truth values
type Env   = [(Name,Bool)]  -- list of pairs: prop name and truth value
                            --  eg ("a", T), ("b", F)

-- showing a proposition
showProp :: Prop -> String
showProp (Var x)   = x
showProp F         = "F"
showProp T         = "T"
showProp (Not p)   = par' ("~" ++ showProp p)
showProp (p :|: q) = par' (showProp p ++ "|" ++ showProp q)
showProp (p :&: q) = par' (showProp p ++ "&" ++ showProp q)

par' :: String -> String
par' s = "(" ++ s ++ ")"

-- names in a proposition
names :: Prop -> Names
names (Var x)   = [x]
names F         = []
names T         = []
names (Not p)   = names p
names (p :|: q) = nub (names p ++ names q)  -- 'nub' is from Data.List
names (p :&: q) = nub (names p ++ names q)  -- nub removes duplicates

-- evaluating a proposition in an environment
-- note the environment 'e' is really only evaluated in the first case
eval :: Env -> Prop -> Bool
eval e (Var x)   = lookUp e x              -- lookup value of Var in Env
eval e F         = False                   -- value of type F
eval e T         = True                    -- value of type T
eval e (Not p)   = not (eval e p)          -- define Not value
eval e (p :|: q) = eval e p || eval e q    -- define :|: value
eval e (p :&: q) = eval e p && eval e q    -- define :&: value

-- lookup values of variables
lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x = the [ y | (x',y) <- xys, x == x' ]
    where
        the [x] = x
        
-- example propositions
p0 :: Prop
p0 = (Var "a" :&: Not (Var "a"))

ep0 :: Env
ep0 = [("a",True)]        

p1 :: Prop
p1 = (Var "a" :&: Var "b") :|:
     (Not (Var "a") :&: Not (Var "b"))
     
ep1 :: Env
ep1 = [("a",False), ("b",False)]

{-
    Example output:
    
        *Main> showProp p0
        (a&(~a))"
        *Main> names p0
        ["a"]
        *Main> eval ep0 p0
        False
        *Main> lookUp ep0 "a"
        True   

        *Main> showProp p1
        "((a&b)|((~a)&(~b)))"       
        *Main> names p1
        ["a","b"]
        *Main> eval ep1 p1
        True
        *Main> lookUp ep1 "a"
        False
        
    A 'contradiction' always evaluates to False regardless of the variables
    A 'tautology' always evaluates to True regardless of the variables
-}

-- All possible environments
envs :: Names -> [Env]
envs []     = [[]]
envs (x:xs) = [ (x,False):e | e <- envs xs ] ++
              [ (x,True ):e | e <- envs xs ]
              
-- alternative definition
envs' :: Names -> [Env]
envs' []     = [[]]
envs' (x:xs) = [ (x,b):e | b <- bs, e <- envs' xs ]
    where
        bs = [False,True]              

-- examples of all possible environments
envs'' = [[]]

envsB  ["b"] = [("b",False):[]] ++ [("b",True ):[]]
envsB1 ["b"] = [[("b",False)], [("b",True )]]

envsAb ["a","b"] = [("a",False):e | e <- envs ["b"] ]
                ++ [("a",True ):e | e <- envs ["b"] ]
                 
envsAb1 ["a","b"] = [("a",False):[("b",False)],("a",False):[("b",True )]] 
                 ++ [("a",True ):[("b",False)],("a",True ):[("b",True )]]

envsAb2 = [[("a",False),("b",False)],
           [("a",False),("b",True )],
           [("a",True ),("b",False)],
           [("a",True ),("b",True )]]        
           
-- satisfiable
--   p is satisfiable (some env where its value is True)
--   if, from all the environments, p is true
satisfiable :: Prop -> Bool
satisfiable p = or [ eval e p | e <- envs (names p) ]

           
p2 :: Prop
p2 = (Var "a" :&: Var "b") :|:
     (Not (Var "a") :&: Not (Var "b"))           
     
{-
    Example output:
    
        *Main> envs (names p2)
        [[("a",False),("b",False)],[("a",False),("b",True)],[("a",True),("b",False)],[("a",True),("b",True)]]
        *Main> [eval e p2 | e <- envs (names p2)]
        [True,False,False,True]
        *Main> satisfiable p2
        True
    
-}     
