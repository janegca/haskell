module W02 where

{-
    Week 02 - Algebraic Data Types
    
    Ref:
    http://www.seas.upenn.edu/~cis194/spring13/lectures/02-ADTs.html
-}

-- Enumeration Types

-- 'Thing' is an 'enumeration type' (and a 'type constructor')
-- Shoe, Ship, SealingWax, etc. are 'data constructors'
data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show
  
shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

-- write functions on Things using pattern matching
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

-- a shorter version of isSmall
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True        -- catch any other Thing

-- Algebraic Data Types other than enumerations

data FailableDouble = Failure           -- ctor with no args
                    | OK Double         -- ctor with one arg, a Double
  deriving Show

ex01 = Failure          -- type is FailableDouble
ex02 = OK 3.4           -- type is FailableDouble

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- data constructors can take multiple arguments

-- Store a person's name, age, and favourite Thing.
data Person = Person String Int Thing       -- type and data ctors can
  deriving Show                             -- have the same name (they
                                            -- are stored in separate
                                            -- namespaces)
brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- pattern matching
{-
    General pattern
    
    pat ::= _
         |  var
         |  var @ ( pat )
         |  ( Constructor pat1 pat2 ... patn )    

-}
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."

-- the underlying structure for pattern matching is the case structure
ex03 = case "Hello" of
           []      -> 3
           ('H':s) -> length s
           _       -> 7
           
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d

-- Recursive Data Types
data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

  