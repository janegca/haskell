-- Informatics 1 Functional Programming
-- December 2013 Class Exam
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/


import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
                        
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

{- 1a
    Write a function 
        f :: String -> Int 
    
    that converts a list of base-3 digits to the corresponding 
    numerical value. For example:
    
    f "201" = (2 * 3^2) + (0 * 3^1) + (1 * 3^0) = 19
    f "12" = 5
    f "1202" = 47
    f "120221" = 430
    
    Use basic functions, list comprehension, and library functions, but 
    not recursion. You may assume that the input is a string of base-3 
    digits. Credit may be given for indicating how you have tested your
    function.
    
    [Hint: Start by reversing the order of the digits in the list. ]
-}

f :: String -> Int
f xs = sum [ digitToInt x * 3^i | (x,i) <- zip (reverse xs) [0..]]

test1a = f "201"    == 19 &&
         f "12"     ==  5 &&
         f "1202"   ==  47 &&
         f "120221" == 430

{- 1b
    Write a second function 
            g :: String -> Int 
    
    that behaves like f, this time using basic functions, library 
    functions and recursion, but not list comprehension. Credit may be 
    given for indicating how you have tested your function.
    
    [Hint: Start by reversing the order of the digits in the list. ]
-}
g :: String -> Int
g xs = conv 0 (reverse xs)
    where
        conv n (c:cs) = (digitToInt c) * 3^n + conv (n+1) cs
        conv _ []     = 0
        
test1b = g "201"    == 19 &&
         g "12"     ==  5 &&
         g "1202"   ==  47 &&
         g "120221" == 430 
        
-- Question 2 --------------------------------------------------------

{- 2a
    Write a function 
        p :: [Int] -> Bool 
        
    that, given a non-empty list of numbers beginning with a non-zero 
    number, returns True if each positive number in the list is
    divisible by the first number in the list. For example:
    
        p [2,6,-3,0,18,-17,10] = True
        p [-13]                = True
        p [-3,6,1,-3,9,18]     = False
        p [5,-2,-6,3]          = False
    
    The function should give an error if applied to an empty list or to
    a list whose first element is zero.
    
    Use basic functions, list comprehension, and library functions, but
    not recursion. Credit may be given for indicating how you have 
    tested your function.
-}
p :: [Int] -> Bool
p []                = error("Empty list")
p (h:t) | h == 0    = error("Zero head")
        | otherwise = divByHead t
    where
        divByHead xs = sum [ x `mod` h | x <- xs, x > 0 ] == 0
        
test2a = p [2,6,-3,0,18,-17,10] == True &&
         p [-13]                == True &&
         p [-3,6,1,-3,9,18]     == False &&
         p [5,-2,-6,3]          == False
        
-- provided solution
p' :: [Int] -> Bool
p' (a:xs) | a /= 0 = and [ x `divBy` a | x <- xs, x >= 0 ]

divBy :: Int -> Int -> Bool
divBy x y = (x `mod` y == 0)
        
{- 2b
    Write a second function 
            q :: [Int] -> Bool 
    
    that behaves like p, this time using basic functions and recursion,
    but not list comprehension or library functions. Credit may be 
    given for indicating how you have tested your function.
-}
q :: [Int] -> Bool
q []              = error("Empty list")
q (h:t ) | h == 0 = error("Zero head")
         | otherwise = (divByHead t) == 0
    where
        divByHead (x:xs) | x > 0     = x `mod` h + divByHead xs
                         | otherwise = divByHead xs
        divByHead [] = 0
                         
test2b = q [2,6,-3,0,18,-17,10] == True &&
         q [-13]                == True &&
         q [-3,6,1,-3,9,18]     == False &&
         q [5,-2,-6,3]          == False
                         

{- 2c
    Write a third function 
            r :: [Int] -> Bool 
    
    that also behaves like p, this time using the following 
    higher-order library functions:
    
        map :: (a -> b) -> [a] -> [b]
        filter :: (a -> Bool) -> [a] -> [a]
        foldr :: (a -> b -> b) -> b -> [a] -> b
    
    Do not use recursion or list comprehension. Credit may be given 
    for indicating how you have tested your function.
-}

r :: [Int] -> Bool
r []                = error ("Empty list")
r (h:t) | h == 0    = error("Zero head")
        | otherwise = divByHead (filter (> 0) t)
    where
        divByHead xs = sum ( map (`mod` h) xs) == 0
        
-- provided solution uses foldr and map
r' :: [Int] -> Bool
r' (a:xs) | a /= 0 = foldr (&&) True 
                       (map (`divBy` a) (filter (>= 0) xs))        
        
test2c = r [2,6,-3,0,18,-17,10] == True &&
         r [-13]                == True &&
         r [-3,6,1,-3,9,18]     == False &&
         r [5,-2,-6,3]          == False

test2 = test2a && test2b && test2c
         
{- Question 3 --------------------------------------------------------

    The following data type represents arithmetic expressions over a 
    single variable:
    
        data Expr = X                 -- variable
                  | Const Int         -- integer constant
                  | Neg Expr          -- negation
                  | Expr :+: Expr     -- addition
                  | Expr :*: Expr     -- multiplication

    The template file includes code that enables QuickCheck to generate
    arbitary values of type Expr, to aid testing.
    
    Note: In this question, you will need to convert integers to and 
          from strings:
                show 234 = "234" and read "234" :: Int = 234.

-}

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

{- 3a

    Reverse Polish Notation (RPN) is a parenthesis-free way of writing 
    arithmetic expressions. Operators follow all of their operands, 
    like so:
    
          X * 3       in RPN is X 3 *
        -(X * 3)      in RPN is X 3 * -
        (5 + -X) * 17 in RPN is 5 X - + 17 *
        (15 + -(7 * (X + 1))) * 3 in RPN is 15 7 X 1 + * - + 3 *
    
    Write a function 
        rpn :: Expr -> [String] 
    
    which converts an expression to its equivalent in RPN. We will 
    represent RPN expressions as lists of strings. For instance, for 
    the third example above:
    
        rpn ((Const 5 :+: Neg X) :*: Const 17)
        
        should produce
        
        ["5", "X", "-", "+", "17", "*"]
    
    Credit may be given for indicating how you have tested your 
    function.

-}
rpn :: Expr -> [String]
rpn (e1 :+: e2) = rpn e1 ++ rpn e2 ++ ["+"]
rpn (e1 :*: e2) = rpn e1 ++ rpn e2 ++ ["*"]
rpn (Neg e)     = rpn e ++ ["-"]
rpn (Const n)   = [show n]
rpn e           = [show e]

testRPN =    rpn ((Const 5 :+: Neg X) :*: Const 17)
          == ["5", "X", "-", "+", "17", "*"]

{- 3 b

    The algorithm for evaluating an RPN expression uses a list for 
    storing intermediate results. Starting with an empty list, and 
    given a value for the variable X, we scan the expression from left 
    to right until it is exhausted:
    
        If the next item is the variable X, add its value to the front 
        of the list.
    
        If the next item is a constant, add it to the front of the list.
        
        If the next item is an operator with n arguments, then remove 
        the first n items from the front of the list, apply the 
        corresponding operation to them, and add the result to the 
        front of the list. If there were fewer than n items on the list,
        then the original expression was ill-formed.
    
        If the next item is something else, then the original 
        expression was ill-formed.
    
    At this point, there should be one item on the list, and that is 
    the result; otherwise the original expression was ill-formed.

    Implement this as a function 
            evalrpn :: [String] -> Int -> Int, 
    
    where the first argument is the RPN expression and the second 
    argument is the  value of X. For example:
    
        evalrpn ["X", "3", "*"] 10                 = 30
        evalrpn ["X", "3", "*", "-"] 20            = -60
        evalrpn ["5", "X", "-", "+", "17", "*"] 10 = -85
        evalrpn ["15", "7", "X", "1", "+", "*", "-", "+", "3", "*"] 2
                    = -18
    
    evalrpn ["X", "3", "*", "-", "+"] 20 should produce an error, 
    because there are not enough items on the list of intermediate 
    values to perform the final addition.
    
    Credit may be given for indicating how you have tested your 
    function. The file includes a function 
            evalExpr :: Expr -> Int -> Int 
    to evaluate expressions, where evalExpr e n produces the value of 
    e when the variable X has value n. Evaluation of an expression
    using evalExpr should produce the same result as evaluation of its
    RPN version using evalrpn.
    
-}
evalrpn :: [String] -> Int -> Int
evalrpn expr n = head (reduce [] expr)
    where
        reduce res (x:xs) 
            | x == "X"  = reduce (n:res) xs
            | x == "-"  = reduce ((-1) * (head res) : tail res) xs
            | x == "+"  = reduce (calc sum res) xs
            | x == "*"  = reduce (calc product res) xs
            | otherwise = reduce ((read x :: Int): res) xs
        reduce res []   = res
            
        calc f rs = if length rs >= 2
                    then f (take 2 rs) : drop 2 rs
                    else error ("ill-formed expression")
                    
-- provided solution uses 'foldl' which reduces adjacent
-- elements according to the rules of the function 'step'
evalrpn' :: [String] -> Int -> Int
evalrpn' s n = the (foldl step [] s)
    where
        step (x:y:ys) "+" = (y+x):ys
        step (x:y:ys) "*" = (y*x):ys
        step (x:ys) "-"   = (-x):ys
        step ys "X"       = n:ys
        step ys m | all (\c -> isDigit c || c=='-') m
                           = (read m :: Int):ys
                  | otherwise = error "ill-formed RPN"

        the :: [a] -> a
        the [x] = x
        the xs  = error "ill-formed RPN"                    
                          
testEval =  evalrpn ["X", "3", "*"] 10                 ==  30 &&
            evalrpn ["X", "3", "*", "-"] 20            == -60 &&
            evalrpn ["5", "X", "-", "+", "17", "*"] 10 == -85 &&
            evalrpn ["15", "7", "X", "1", "+", "*", 
                     "-", "+", "3", "*"] 2             == -18

testEvalFail = evalrpn ["X", "3", "*", "-", "+"] 20
             
-- quickCheck test             
prop_eval  e n = evalExpr e n == evalrpn  (rpn e) n
prop_eval' e n = evalExpr e n == evalrpn' (rpn e) n

              