-- Informatics 1 - Functional Programming 
-- Tutorial Review - 6 - Algebraic Data Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#lectures
--
-- Notes:
--      Turns out you can define your own operators in Haskell
--      see the provided solution to question 1b
--
--      Still thinking along imperative lines; need to spend
--      more time to work out how imperative patterns can be
--      converted to FP patterns

import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below

-- Question 1

data Expr = Var String
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Expr

instance Arbitrary Expr where
  arbitrary = sized arb
    where
    arb 0          =  liftM Var arbitrary
    arb n | n > 0  =  oneof [liftM Var arbitrary,
                             liftM2 (:+:) sub sub, 
                             liftM2 (:*:) sub sub] 
      where
      sub = arb (n `div` 2)

{- 1a ------------------------------------------------------------------
    Write two functions 
        isTerm, isNorm :: Expr -> Bool 
    
    that return true when the given expression is a term or is normal, 
    respectively. We say that an expression is a term if it is a product
    of variables, that is, it is a variable or the product of two 
    expressions that are terms. We say that an expression is normal if 
    it is a sum of terms, that is, if it is a term or the sum of two 
    expressions that are normal.

    [Implementation Note:
        if a pattern matches a constructor, the input is 'de-constructed'
        
        Example walkthrough
        
          isTerm ((Var "x" :*: Var "y") :*: Var "z")
        = isTerm (Var "x" :*: Var "y") && isTerm Var "z"
        = (isTerm Var "x" && isTerm Var "y") && isTerm Var "z"
        = True && True && True
        = True
        
        here, the 'pattern' match is on (a :*: b) where
        a becomes (Var "x" :*: Var "y"), and
        b becomes Var "z"
       
    ]
-}
--
-- provided solution
--
isNorm :: Expr -> Bool
isNorm (a :+: b) = isNorm a && isNorm b
isNorm a         = isTerm a

isTerm :: Expr -> Bool
isTerm (Var a)   = True
isTerm (a :+: b) = False
isTerm (a :*: b) = isTerm a && isTerm b

testIsTerm =
       isTerm (Var "x") == True
    && isTerm ((Var "x" :*: Var "y") :*: Var "z")  == True
    && isTerm ((Var "x" :*: Var "y") :+: Var "z")  == False
    && isTerm (Var "x"  :*: (Var "y" :+: Var "z")) == False
    
testIsNorm =
       isNorm (Var "x") == True
    && isNorm (Var "x" :*: Var "y" :*: Var "z") == True
    && isNorm ((Var "x" :*: Var "y") :+: Var "z") == True
    && isNorm (Var "x" :*: (Var "y" :+: Var "z")) == False
    && isNorm ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z")) == True
    && isNorm ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y")) == False
    && isNorm (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+:
       ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y"))) == True
    
-- provided tests    
prop_isTerm e   = isTerm e ==> isNorm e
prop_isNorm e f = isNorm e && isNorm f ==> isNorm (e :+: f)
    
{- 1b ------------------------------------------------------------------
    Write a function 
        norm :: Expr -> Expr 
        
    that converts an expression to an equivalent expression in normal 
    form. An expression not in normal form may be converted to normal 
    form by repeated application of the distributive laws,
    
            (a + b) x c  = (a x c) + (b x c)
             a x (b + c) = (a x b) + (a x c)

-}
norm :: Expr -> Expr
norm ((a :+: b) :*: (c :+: d))   = norm (a :*: ( c :+: d)) :+: 
                                   norm (b :*: (c :+: d))
norm (a :*: (b :+: c))           = ((a :*: b) :+: (a :*: c))
norm e                           = e

testNorm =
       norm (Var "x") == (Var "x")
    && norm ((Var "x" :*: Var "y") :*: Var "z")
       ==   ((Var "x" :*: Var "y") :*: Var "z")
    && norm ((Var "x" :*: Var "y") :+: Var "z")
       == ((Var "x" :*: Var "y") :+: Var "z")
    && norm (Var "x" :*: (Var "y" :+: Var "z"))
       == ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z"))
    && norm ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y"))
       == (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+:
          ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y")))
          
-- provided quick test  
--   implementation passes testNorm but fails quickChecks
--   when expressions are nested; see provided solution below
prop_norm a  =  isNorm (norm a) && norm (norm a) == norm a
          
{- The provided solution for this question (shown below) uses
   an operator (***) which is defined in the 'where' clause
   
   To test that this is the case, run the 'testB' 
   function which also defines '***' in a 'where' clause
   whose output for 'testB 2 4' = 64

   *Main> testB 2 4
    64

-}         
testB a b = a *** b
    where
        a *** b = a^2 * b^2

norm' :: Expr -> Expr
norm' (Var v)            =  Var v
norm' (a :+: b)          =  norm' a :+: norm' b
norm' (a :*: b)          =  norm' a *** norm' b
  where
  (a :+: b) *** c       =  (a *** c) :+: (b *** c)
  a *** (b :+: c)       =  (a *** b) :+: (a *** c)
  a *** b               =  a :*: b          

testNorm' =
       norm' (Var "x") == (Var "x")
    && norm' ((Var "x" :*: Var "y") :*: Var "z")
       ==   ((Var "x" :*: Var "y") :*: Var "z")
    && norm' ((Var "x" :*: Var "y") :+: Var "z")
       == ((Var "x" :*: Var "y") :+: Var "z")
    && norm' (Var "x" :*: (Var "y" :+: Var "z"))
       == ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z"))
    && norm' ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y"))
       == (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+:
         ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y")))
    
prop_norm' a  =  isNorm (norm' a) && norm' (norm' a) == norm' a  
  
-- Question 2 ---------------------------------------------------------
{- 2a
    A scalar is a single integer, and a vector is a pair of integers.
        type Scalar = Int
        type Vector = (Int,Int)

    Write functions
        add :: Vector -> Vector -> Vector
        mul :: Scalar -> Vector -> Vector

    that add two vectors by adding corresponding components of the
    vectors, and multiply a scalar and a vector by multiplying each 
    component of the vector by the scalar. For example,
    
        add (1,2) (3,4) == (4,6)
        mul 2 (3,4)     == (6,8)
        
-}

type Scalar = Int
type Vector = (Int,Int)

add :: Vector -> Vector -> Vector
add (a,b) (c,d) = (a+c, b+d)

mul :: Scalar -> Vector -> Vector
mul s (x,y) = (s*x, s*y)

test2a =  add (1,2) (3,4) == (4,6)
       && mul 2 (3,4)     == (6,8) 
       
{- 2b
    The following data type represents terms that compute vectors. A 
    term is a vector consisting of two scalars, the sum of two terms, 
    or the multiplication of a scalar by a term.
    
        data Term = Vec Scalar Scalar
                  | Add Term Term
                  | Mul Scalar Term

    Write a function 
        eva :: Term -> Vector 
        
    that takes a term and computes the corresponding vector. 
    For example,
        eva (Vec 1 2)                                 == (1,2)
        eva (Add (Vec 1 2) (Vec 3 4))                 == (4,6)
        eva (Mul 2 (Vec 3 4))                         == (6,8)
        eva (Mul 2 (Add (Vec 1 2) (Vec 3 4)))         == (8,12)
        eva (Add (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4))) == (8,12)
        
-}

data Term  =  Vec Scalar Scalar
            | Add Term Term
            | Mul Scalar Term
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of 
-- type Term

instance Arbitrary Term where
  arbitrary = sized arb
    where
    arb 0          =  liftM2 Vec arbitrary arbitrary
    arb n | n > 0  =  oneof [liftM2 Vec arbitrary arbitrary,
                             liftM2 Add sub sub, 
                             liftM2 Mul arbitrary sub] 
      where
      sub = arb (n `div` 2)

eva :: Term -> Vector
eva (Vec x y) = (x,y)
eva (Add a b) = add (eva a) (eva b)
eva (Mul a b) = mul a (eva b)

testEva =  eva (Vec 1 2)                                 == (1,2)
        && eva (Add (Vec 1 2) (Vec 3 4))                 == (4,6)
        && eva (Mul 2 (Vec 3 4))                         == (6,8)
        && eva (Mul 2 (Add (Vec 1 2) (Vec 3 4)))         == (8,12)
        && eva (Add (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4))) == (8,12)
        
-- from provided solution, how to test Mul??        
prop_evaAdd t t' = eva (Add t t') == eva (Add t' t) 

{- 2c
    Write a function 
        sho :: Term -> String 
    that converts a term to a string. Vectors should be printed as a
    pair of integers in parentheses, sums and products should be 
    written infix surrounded by parentheses. For example,
    
        sho (Vec 1 2)                         == "(1,2)"
        sho (Add (Vec 1 2) (Vec 3 4))         == "((1,2)+(3,4))"
        sho (Mul 2 (Vec 3 4))                 == "(2*(3,4))"
        sho (Mul 2 (Add (Vec 1 2) (Vec 3 4))) == "(2*((1,2)+(3,4)))"
        sho (Add (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))
             == "((2*(1,2))+(2*(3,4)))"

    You may use the show function on scalars in your definition, but 
    not the show function on terms that is provided in the template
    file for use by QuickCheck.
-}

sho :: Term -> String
sho (Vec x y) = show (x,y)
sho (Add a b) = "(" ++ sho a ++ "+" ++ sho b ++ ")"
sho (Mul a b) = "(" ++ show a ++ "*" ++ sho b ++ ")"

testSho =  sho (Vec 1 2)                         == "(1,2)"
        && sho (Add (Vec 1 2) (Vec 3 4))         == "((1,2)+(3,4))"
        && sho (Mul 2 (Vec 3 4))                 == "(2*(3,4))"
        && sho (Mul 2 (Add (Vec 1 2) (Vec 3 4))) == "(2*((1,2)+(3,4)))"
        && sho (Add (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))
                   == "((2*(1,2))+(2*(3,4)))"

-- Question 3 --------------------------------------------------------
{-
    We introduce a data type to represent collections of points in a 
    grid:
        type Point = (Int,Int)
        data Points = Rectangle Point Point
                    | Union Points Points
                    | Difference Points Points

    The grid starts with (0,0) in the top left corner. The first 
    coordinate of a point represents the horizontal distance from the
    origin, the second represents the vertical distance.
    
    The constructor Rectangle selects all points in a rectangular area. 
    For example,
        Rectangle (0,0) (2,1)
        
    gives the top left and bottom right corners of a rectangle, and 
    represents all the points in between (inclusive):
        (0,0) (1,0) (2,0)
        (0,1) (1,1) (2,1)
        
    Secondly, Union combines two collections of points; for example,
        Union (Rectangle (0,0) (1,1)) (Rectangle (1,0) (2,1))
        
    represents the same collection of points as above. 
    
    Finally, the constructor Difference selects those points that are 
    in the first collection but not in the second. For example:
        Difference (Rectangle (0,0) (2,2)) (Rectangle (0,2) (3,2))
        
    again gives the same collection of points as above.

-}
type Point = (Int,Int)
data Points = Rectangle Point Point
            | Union Points Points
            | Difference Points Points
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Points

instance Arbitrary Points where
  arbitrary = sized arb
    where
    arb 0          =  liftM2 Rectangle arbitrary arbitrary
    arb n | n > 0  =  oneof [liftM2 Rectangle arbitrary arbitrary,
                             liftM2 Union sub sub, 
                             liftM2 Difference sub sub] 
      where
      sub = arb (n `div` 2)

{- 3a
    Write a function
        inPoints :: Point -> Points -> Bool
        
    to determine whether a point is in a given collection.
-}

-- based on provided solution
inPoints :: Point -> Points -> Bool
inPoints (x,y) (Rectangle (l,t) (r,b))
    = l <= x && x <= r && t <= y && y <= b
inPoints p (Union a b)      = inPoints p a || inPoints p b
inPoints p (Difference a b) = inPoints p a && not (inPoints p b)

testInPts =  inPoints (1,1) (Rectangle (0,0) (2,1)) == True
          && inPoints (3,4) (Rectangle (0,0) (2,1)) == False
          && inPoints (1,1) (Union (Rectangle (0,0) (0,1))
               (Rectangle (1,0) (1,1))) == True
          && inPoints (2,2) (Union (Rectangle (0,0) (0,1))
               (Rectangle (1,0) (1,1))) == False
          && inPoints (1,1) (Difference (Rectangle (0,0) (1,1))
               (Rectangle (0,0) (0,1))) == True
          && inPoints (0,0) (Difference (Rectangle (0,0) (1,1))
              (Rectangle (0,0) (0,1))) == False
    
-- provided quick test
prop_inPoints p ps ps'  =
   inPoints p ps ==> ( inPoints p (Union ps ps') &&
                       inPoints p (Union ps' ps) &&
                       not (inPoints p (Difference ps' ps)) )

{- 3b
    Write a function
        showPoints :: Point -> Points -> [String]

    to show a collection of points as a list of strings, representing 
    the points on a grid. The grid starts with (0,0) in the top left 
    corner, while the bottom right corner, which determines the size of
    the grid, is given by the first argument to the function showPoints.
    
    The strings in the list that is returned should correspond to the 
    rows (not the columns) of the grid. Use an asterisk ('*') to 
    represent a point, and use blank space (' ') to fill out the lines.
    
-}

showPoints :: Point -> Points -> [String]
showPoints (h,v) rect 
    = display [showChr (r,c) | c <- [0..v], r <- [0..h]]
    where
        width     = h + 1
        
        showChr p = if inPoints p rect then "*" else " "
                
        display [] = []                
        display xs = concat(take width xs) : display (drop width xs)
                         
testShowPts =  showPoints (4,2) (Rectangle (1,1) (3,3)) ==
                ["     ",
                 " *** ",
                 " *** "]
            && showPoints (5,2) (Difference (Rectangle (0,0) (4,1))
                (Rectangle (2,0) (2,2))) ==
                ["** ** ",
                 "** ** ",
                 "      "]

-- provided solution -- avoids the 'concat'
showPoints' :: Point -> Points -> [String]
showPoints' (a,b) ps = [ makeline y | y <- [0..b] ]
    where
      makeline y = [ if inPoints (x,y) ps then '*' else ' ' |
                     x <- [0..a] ]
                 