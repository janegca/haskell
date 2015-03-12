{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M

{-
    Exercise 1

    Write Version 1 of the calculator: an evaluator for ExprT, with the
    signature

        eval :: ExprT -> Integer

    For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
-}
expr1 :: ExprT
expr1 = (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

ex1test :: Bool
ex1test = eval expr1 == 20

{-
    Exercise 2

    The UI department has internalized the focus group data and is
    ready to synergize with you. They have developed the front-facing
    user-interface: a parser that handles the textual representation of the
    selected language. They have sent you the module Parser.hs, which
    exports parseExp, a parser for arithmetic expressions. If you pass
    the constructors of ExprT to it as arguments, it will convert Strings
    representing arithmetic expressions into values of type ExprT. For
    example:

        *Calc> parseExp Lit Add Mul "(2+3)*4"
        Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
        *Calc> parseExp Lit Add Mul "2+3*4"
        Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
        *Calc> parseExp Lit Add Mul "2+3*"
        Nothing

    Leverage the assets of the UI team to implement the value-added
    function

        evalStr :: String -> Maybe Integer

    which evaluates arithmetic expressions given as a String, producing
    Nothing for inputs which are not well-formed expressions, and
    Just n for well-formed inputs that evaluate to n.

-}
evalStr :: String -> Maybe Integer
evalStr xs = f (parseExp Lit Add Mul xs)
    where
        f Nothing  = Nothing
        f (Just e) = Just (eval e)
 
ex2a, ex2b, ex2c :: Maybe Integer
ex2a = evalStr "(2+3)*4"
ex2b = evalStr "2+3*4"
ex2c = evalStr "2+3*"
             
{-

    Exercise 3

    Good news! Early customer feedback indicates that people really
    do love the interface! Unfortunately, there seems to be some 
    disagreement over exactly how the calculator should go about its 
    calculating business. The problem the software department (i.e. you) 
    has is that while ExprT is nice, it is also rather inflexible, which
    makes catering to diverse demographics a bit clumsy. You decide to 
    abstract away the properties of ExprT with a type class.
    
    Create a type class called Expr with three methods called lit, add,
    and mul which parallel the constructors of ExprT. Make an instance of
    Expr for the ExprT type, in such a way that

        mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
            == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

    Think carefully about what types lit, add, and mul should have. It
    may be helpful to consider the types of the ExprT constructors, which
    you can find out by typing (for example)

        *Calc> :t Lit
        at the ghci prompt.

    Remark. Take a look at the type of the foregoing example expression:

       *Calc> :t mul (add (lit 2) (lit 3)) (lit 4)
        Expr a => a
 
    What does this mean? The expression mul (add (lit 2) (lit 3)) (lit 4)
    has any type which is an instance of the Expr type class. So writing it
    by itself is ambiguous: GHC doesn’t know what concrete type you
    want to use, so it doesn’t know which implementations of mul, add,
    and lit to pick.
    
    One way to resolve the ambiguity is by giving an explicit type
    signature, as in the above example. Another way is by using such an
    expression as part of some larger expression so that the context in
    which it is used determines the type. For example, we may write a
    function reify as follows:
    
        reify :: ExprT -> ExprT
        reify = id
    
    To the untrained eye it may look like reify does no actual work!
    But its real purpose is to constrain the type of its argument to ExprT.

    Now we can write things like
        reify $ mul (add (lit 2) (lit 3)) (lit 4)
    
    at the ghci prompt.

-}
class Expr a where
    lit :: Integer -> a
    add :: a  -> a -> a
    mul :: a  -> a -> a

instance Expr ExprT where
    lit n     = Lit n
    add e1 e2 = Add e1 e2
    mul e1 e2 = Mul e1 e2
    
reify :: ExprT -> ExprT
reify = id    

ex3test :: Bool
ex3test = reify (mul (add (lit 2) (lit 3)) (lit 4))
       == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

{-
    Exercise 4
    
    The point of our Expr type class is that we can now write down
    arithmetic expressions once and have them interpreted in various
    ways just by using them at various types.    

    Make instances of Expr for each of the following types:
    • Integer — works like the original calculator
    • Bool    — every literal value less than or equal to 0 is interpreted
                as False, and all positive Integers
                are interpreted as True; “addition” is logical or,
                “multiplication” is logical and
    • MinMax  — “addition” is taken to be the max function, while
                “multiplication” is the min function
    • Mod7    — all values should be in the ranage 0 . . . 6, and
                all arithmetic is done modulo 7; for example,
                5 + 3 = 1.

    The last two variants work with Integers internally, but in order
    to provide different instances, we wrap those Integers in newtype
    wrappers. These are used just like the data constructors we’ve seen
    before.

        newtype MinMax = MinMax Integer deriving (Eq, Show)
        newtype Mod7 = Mod7 Integer deriving (Eq, Show)

    Once done, the following code should demonstrate our family of
    calculators:

        testExp :: Expr a => Maybe a
        testExp     = parseExp lit add mul "(3 * -4) + 5"
        
        testInteger = testExp :: Maybe Integer
        testBool    = testExp :: Maybe Bool
        testMM      = testExp :: Maybe MinMax
        testSat     = testExp :: Maybe Mod7
        
    Try printing out each of those tests in ghci to see if things are
    working. It’s great how easy it is for us to swap in new semantics for
    the same syntactic expression!        
-}       

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Integer where
    lit n     = n
    add x y   = x + y
    mul x y   = x * y
    
testInteger :: Maybe Integer
testInteger = testExp  

instance Expr Bool where
    lit n   = n > 0
    add x y = x || y
    mul x y = x && y
    
testBool :: Maybe Bool
testBool = testExp    

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit n                     = MinMax n
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    
testMM :: Maybe MinMax
testMM = testExp
    
newtype Mod7   = Mod7 Integer deriving (Eq, Show)
    
instance Expr Mod7 where
    lit n                 = Mod7 (mod n 7)
    add (Mod7 x) (Mod7 y) = lit (x + y)
    mul (Mod7 x) (Mod7 y) = lit (x * y)

testExp2 :: Expr a => Maybe a  
testExp2 = parseExp lit add mul "5 + 3"    

testSat, testSata :: Maybe Mod7    
testSat  = testExp
testSata = testExp2

{-
    Exercise 5 

    The folks down in hardware have finished our new custom CPU,
    so we’d like to target that from now on. The catch is that a stackbased
    architecture was chosen to save money. You need to write a
    version of your calculator that will emit assembly language for the
    new processor.

    The hardware group has provided you with StackVM.hs, which
    is a software simulation of the custom CPU. The CPU supports six
    operations, as embodied in the StackExp data type:
    
        data StackExp = PushI Integer
                      | PushB Bool
                      | Add
                      | Mul
                      | And
                      | Or
                      deriving Show
        
        type Program = [StackExp]

    PushI and PushB push values onto the top of the stack, which can
    store both Integer and Bool values. Add, Mul, And, and Or each pop
    the top two items off the top of the stack, perform the appropriate
    operation, and push the result back onto the top of the stack. For
    example, executing the program
    
        [PushB True, PushI 3, PushI 6, Mul]
    
    will result in a stack holding True on the bottom, and 18 on top of
    that.     

    If there are not enough operands on top of the stack, or if an operation
    is performed on operands of the wrong type, the processor
    will melt into a puddle of silicon goo. For a more precise specification
    of the capabilities and behavior of the custom CPU, consult the
    reference implementation provided in StackVM.hs.
    
    Your task is to implement a compiler for arithmetic expressions.
    Simply create an instance of the Expr type class for Program, so that
    arithmetic expressions can be interpreted as compiled programs. For
    any arithmetic expression exp :: Expr a => a it should be the case
    that
    
        stackVM exp == Right [IVal exp]
    
    Note that in order to make an instance for Program (which is a
    type synonym) you will need to enable the TypeSynonymInstances
    language extension, which you can do by adding
    
        {-# LANGUAGE TypeSynonymInstances #-}
        
    as the first line in your file.
    
    Finally, put together the pieces you have to create a function    

        compile :: String -> Maybe Program

    which takes Strings representing arithmetic expressions and compiles
    them into programs that can be run on the custom CPU.
-}
instance Expr S.Program where
    lit  n    = [S.PushI n]
    add  x y  = x ++ y ++ [S.Add]
    mul  x y  = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul
   
testCalcI, testCalcB :: Maybe S.Program   
testCalcI = compile "3 * 6"       -- Just [PushI 3,PushI 6,Mul]
testCalcB = compile "True 3 * 6"  -- Nothing
 
-- [Note: can't actually run the resulting program using S.stackVM
--        as wants S.PushI, S.Mul] 
 
{-
    Exercise 6
    
    Some users of your calculator have requested the ability to give
    names to intermediate values and then reuse these stored values
    later.
    
    To enable this, you first need to give arithmetic expressions the
    ability to contain variables. Create a new type class HasVars a which
    contains a single method var :: String -> a. Thus, types which are
    instances of HasVars have some notion of named variables.
    
    Start out by creating a new data type VarExprT which is the same
    as ExprT but with an extra constructor for variables. Make VarExprT
    an instance of both Expr and HasVars. You should now be able to
    write things like
    
        *Calc> add (lit 3) (var "x") :: VarExprT
    
    But we can’t stop there: we want to be able to interpret expressions
    containing variables, given a suitable mapping from variables
    to values. For storing mappings from variables to values, you should
    use the Data.Map module. Add
    
        import qualified Data.Map as M    
        
    at the top of your file. The qualified import means that you must
    prefix M. whenever you refer to things from Data.Map. This is standard
    practice, since Data.Map exports quite a few functions with
    names that overlap with names from the Prelude. Consult the
    Data.Map documentation to read about the operations that are supported
    on Maps.
    
    Implement the following instances:

        instance HasVars (M.Map String Integer -> Maybe Integer)
        instance Expr (M.Map String Integer -> Maybe Integer)

    The first instance says that variables can be interpreted as functions
    from a mapping of variables to Integer values to (possibly)
    Integer values. It should work by looking up the variable in the
    mapping.

    The second instance says that these same functions can be interpreted
    as expressions (by passing along the mapping to subexpressions
    and combining results appropriately).    
    
    Note: to write these instances you will need to enable the FlexibleInstances
    language extension by putting
    
    {-# LANGUAGE FlexibleInstances #-}
    
    as the first line in your file.
    Once you have created these instances, you should be able to test
    them as follows:
    
        withVars :: [(String, Integer)]
        -> (M.Map String Integer -> Maybe Integer)
        -> Maybe Integer
        withVars vs exp = exp $ M.fromList vs
        *Calc> :t add (lit 3) (var "x")
        add (lit 3) (var "x") :: (Expr a, HasVars a) => a
        *Calc> withVars [("x", 6)] $ add (lit 3) (var "x")
        Just 9
        *Expr> withVars [("x", 6)] $ add (lit 3) (var "y")
        Nothing
        *Calc> withVars [("x", 6), ("y", 3)]
        $ mul (var "x") (add (var "y") (var "x"))
        Just 54    
        
    Ref: https://github.com/pdswan/cis194/blob/master/hw5/VarExprT.hs
-}
    
class HasVars a where
    var :: String -> a
    
data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var
      
instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul
    
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- below are two implementations; both work    
{-    

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit       = const . Just
    add f1 f2 = \vm -> do a <- f1 vm
                          b <- f2 vm
                          let res = a + b
                          return res
    mul f1 f2 = \vm -> do a <- f1 vm
                          b <- f2 vm
                          let res = a * b
                          return res                                                    
-}   

-- source: github southp/cs194/h25/Ex6.hs
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x _   = Just x
    mul x y z = case (x z, y z) of
                    (Nothing, _) -> Nothing
                    (_, Nothing) -> Nothing
                    (Just a, Just b) -> Just (a * b)
    add x y z = case (x z, y z) of
                    (Nothing, _) -> Nothing
                    (_, Nothing) -> Nothing
                    (Just a, Just b) -> Just (a + b)     

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs ex = ex $ M.fromList vs    

ex6a, ex6b, ex6c :: Maybe Integer
ex6a = withVars [("x", 6)] $ add (lit 3) (var "x")
ex6b = withVars [("x", 6)] $ add (lit 3) (var "y")
ex6c = withVars [("x", 6), ("y", 3)] 
     $ mul (var "x") (add (var "y") (var "x")) 
     
{-
    How does withVars work with the instance declarations?
    
        ex6a = withVars [("x", 6)] $ add (lit 3) (var "x")
        
        the first argument is a list of type [(String, Integer)]
        the second parameter has type 
            (M.Map String Integer -> Maybe Integer)
        which the second argument must match
            
        the value of the second argument: add (lit 3) (var "x")
        has the type (Expr a, HasVars a) => a
        which means 'a' has to be a member of both types and we
        have instances of type (M.Map String Integer -> Maybe Integer)
        for both Expr and HasVars; so it is a valid argument.
        
        Now, the caller has the ($) operator in front of 
        add (lit 3) (var "x") so it must be processed first.
        Evaluation is right to left so (var "x") is processed first
        and we must use the instance declaration constrained to the
        type (M.Map String Integer -> Maybe Integer), so,
        
                add (lit 3) (var "x")
            ->  add (lit 3) (M.lookup "x")
            
        This gives us a partial application of 'add' since there
        is no argument for (M.lookup "x"). With lazy evaluation in play (??)
        we don't need the (lit 3) value yet, so what gets passed to
        withVars is really:
        
            withVars [("x",6)] (add (lit 3) (M.lookup "x"))
        ->  add (lit 3) (M.lookup "x") $ M.fromList [("x",6)]
        ->  (add (lit 3) (M.lookup "x") fromList [("x",6)]
            
        and execution proceeds, using the
        (M.Map String Integer -> Maybe Integer) instance of Expr
        
        add   (lit 3) (M.lookup "x") fromList [("x",6)]
     -> case (x z, y z)
     -> case ( lit 3 fromList[("x",6)], M.lookup "x" fromList [("x",6)]) 
     -> case ( Just 3, Just 6 ) )
     -> Just (3 + 6)
     -> Just 9
 
-}    
