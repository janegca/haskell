-- Informatics 1 - Functional Programming 
-- Lecture 9 - Algebraic Data Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect09.pdf
--  http://groups.inf.ed.ac.uk/vision/VIDEO/2014/inf1fp.htm
--      video 13/10/2014 23:00 minute mark
--      video 14/10/2014
--      video 22/10/2013 27:00 minute mark 

{-
    ALGEBRAIC DATA TYPES
        Almost every type is an algebraic type and they can be combined 
        to build other types and there are rules for how they are combined.
        
        We can define (build) our own types.
        
        Example types:
        
            data Bool       = False | True
            data Season     = Winter | Spring | Summer | Fall
            data Shape      = Circle Float | Rectangle Float Float
            data List a     = Nil | Cons a (List a)
            data Nat        = Zero | Succ Nat
            data Exp        = Lit Int | Add Exp Exp | Mul Exp Exp
            data Tree a     = Empty | Leaf a | Branch (Tree a) (Tree a)
            data Maybe a    = Nothing | Just a
            data Pair a b   = Pair a b
            data Either a b = Left a | Right b        

-}
{-
    Boolean Type
    
        data Bool = False | True
        
    All types start out with the word 'data'
    A type name which MUST start with a Capital Letter
    Followed by an equal sign, and 
    A list of alternative values known as constructors, which must
        also start with a Captial Letter
        
    The representation of Bool is what is given: False or True
    It is NOT represented in terms of another type ie. False is not
    equal to an Int value 0 or True to 1. Rather, False is equal
    to itself and True is equal to itself.
        
    For Bool the constructors (possible values) are: False or True
    
    Constructors can be used in expressions:
    
        not :: Bool -> Bool                 -- logical Not
        not False = True
        not True  = False
        
        (&&) :: Bool -> Bool -> Bool        -- logical And
        False && q = False
        True  && q = q
        
        (||) :: Bool -> Bool -> Bool        -- logical Or
        False || q = q
        True  || q = True    
        
        eqBool :: Bool -> Bool -> Bool      -- truth table
        eqBool False False = True
        eqBool False True  = False
        eqBool True  False = False
        eqBool True  True  = True

        showBool :: Bool -> String          -- display (print)
        showBool False = "False"
        showBool True  = "True"            
-}
{-
    Seasons
        Similar to Bool but with 4 constructors; 4 distinct
        values
    
-}
data Season = Winter | Spring | Summer | Fall

next :: Season -> Season        -- given one season, return the next
next Winter = Spring
next Spring = Summer
next Summer = Fall
next Fall   = Winter

eqSeason :: Season -> Season -> Bool    -- compare seasons
eqSeason Winter Winter = True
eqSeason Spring Spring = True
eqSeason Summer Summer = True
eqSeason Fall Fall     = True
eqSeason x y           = False

showSeason :: Season -> String          -- display seasons
showSeason Winter = "Winter"
showSeason Spring = "Spring"
showSeason Summer = "Summer"
showSeason Fall   = "Fall"

-- converting between seasons and numbers
-- don't have to do this but it is possible
toInt :: Season -> Int
toInt Winter = 0
toInt Spring = 1
toInt Summer = 2
toInt Fall = 3

fromInt :: Int -> Season
fromInt 0 = Winter
fromInt 1 = Spring
fromInt 2 = Summer
fromInt 3 = Fall

next' :: Season -> Season
next' x = fromInt ((toInt x + 1) `mod` 4)

eqSeason' :: Season -> Season -> Bool
eqSeason' x y = (toInt x == toInt y)

{-
    SHAPE
-}
-- type synonyms
type Radius = Float
type Width  = Float
type Height = Float

-- has two values (constructors) which take arguments
data Shape = Circle Radius
           | Rect   Width Height
           
area :: Shape -> Float
-- calculate the area of a Shape
area (Circle r) = pi * r^2
area (Rect w h) = w * h

--
-- Shape equals and show
--
eqShape :: Shape -> Shape -> Bool
eqShape (Circle r) (Circle r')  = (r == r')
eqShape (Rect w h) (Rect w' h') = (w == w') && (h == h')
eqShape x y = False

showShape :: Shape -> String
showShape (Circle r) = "Circle " ++ showF r
showShape (Rect w h) = "Rect " ++ showF w ++ " " ++ showF h

showF :: Float -> String
showF x | x >= 0 = show x
        | otherwise = "(" ++ show x ++ ")"

--
-- Shape tests and selectors
--
isCircle :: Shape -> Bool
isCircle (Circle r) = True
isCircle (Rect w h) = False

isRect :: Shape -> Bool
isRect (Circle r) = False
isRect (Rect w h) = True

radius :: Shape -> Float
radius (Circle r) = r

width :: Shape -> Float
width (Rect w h) = w

height :: Shape -> Float
height (Rect w h) = h        

-- what's happening behind the scenes
area' :: Shape -> Float
area' s =
    if isCircle s then
        let
            r = radius s
        in
            pi * r^2
    else if isRect s then
        let
            w = width s
            h = height s
        in
            w * h
    else error "impossible"
    
{-
    LISTS 
        - roughly how they could be defined in they were not
          already provided in Haskell

-}    

data List a = Nil
            | Cons a (List a)       -- recursion in the type definition
            
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

{- hide as creates ambiguous error]
(++) :: [a] -> [a] -> [a]           -- infix operator for 'append'
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)
-}

{-
    NATURAL NUMBERS
-}
data Nat = Zero
         | Succ Nat
         
power :: Float -> Nat -> Float
power x Zero = 1.0
power x (Succ n) = x * power x n

add :: Nat -> Nat -> Nat
add m Zero = m
add m (Succ n) = Succ (add m n)

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (Succ n) = add (mul m n) m

{- what infix operator declarations would look like

    (^^) :: Float -> Int -> Float
    x ^^ 0 = 1.0
    x ^^ n = x * (x ^^ (n-1))

    (+) :: Int -> Int -> Int
    m + 0 = m
    m + n = (m + (n-1)) + 1

    (*) :: Int -> Int -> Int
    m * 0 = 0
    m * n = (m * (n-1)) + m
-}

