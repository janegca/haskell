module W04 where

{-
    Week 04 - Higher-order programming and type inference
    
    Ref:
        http://www.seas.upenn.edu/~cis194/fall14/spring13/
            lectures/04-higher-order.html
-}
{-
    Anonymous functions or 'lambda abstractions' are used to
    write, short, one off, unnamed functions.
    
    eg  (\x -> x > 100)
        (\x y z -> [x,2*y,3*z])
        
    A lambda instruction begins with a '\' (to represent the 
    lambda character) arguments, the lambda operator (->)
    followed by the body of the function
    
    We can also use 'sections', partially applied operations,
    For example, the lambda abstraction (\x -> x > 100) can
    be written as (> 100). We can partially apply, or create a
    'section', using any binary operator: (* 6), (+ 1), (100 >);
    implicit, to each is, a full lambda expression:
        (* 6)  is (\x -> x * 6)
        (+ 1)  is (\x -> x + 1)
        (100 >) is (\x -> 100 > x)
-}
{-
    Function Composition (.)
    
    The (.) operator is used for 'function composition'
        f . g = f (g x)
        
    and has the type
    
        (.) :: (b -> c) -> (a -> b) -> a -> c
        
    It allows us to easily chain together a series of operations
    to create a 'pipeline' of smaller functions. The pipeline
    is executed right to left.
    
    For example,
    
-}
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (> 100) xs

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

-- can be rewritten as
myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

{-
    Currying and partial application
    
    All functions in Haskell take only one argument.
    A function like
        f :: Int -> Int -> Int
        f x y = 2*x + y
        
    Takes one argument, an Int, and, really, returns another
    function of type (Int -> Int); the parentheses are implied
    in the type signature as function arrows (->) are 'right'
    associative
    
        W -> X -> Y -> Z  is read as W -> (X -> (Y -> Z))
        
    While function application is 'left-associative'; 
        
        f 3 2  is read as (f 3) 2
        
    here, 'f' is applied to 3 and a new function of type Int -> Int
    is returned.
            f 3 2
        -> (f 3) 2
        -> (2*3 + y) 2
        
    The new function (2*3 + y) has type Int -> Int and it will
    be applied to the remaining argument.
    
    Functions that take multiple-arguments can be expressed as
    lambda abstractions:
    
        \ x y z -> ... is equivalent to \x -> (\y -> (\z -> ...))
        
    The idea of representing multi-argument functions as single
    operator functions that return another function is called
    'currying' after Haskell Curry although it was first conceived
    by Moses Schönfinkel.
    
    If we want actually represent a function as having two arguments,
    we can use a tuple
    
-}
-- force evaluation to an Int vs a function of type (Int -> Int)
f'' :: (Int, Int) -> Int
f'' (x,y) = 2*x + y

{-
    Haskell defines two functions: curry and uncurry, allowing
    us to convert between the two
    
        curry   f  x y  = f (x,y)
        uncurry f (x,y) = f x y
        
    We can uncurry a pair to apply a function to both elements
    
        uncurry (+) (2,3)  => 5
        
    The idea of currying makes partial application of a function
    extremely easy; we can apply (fix) the first argument to get back
    a new function of fewer arguments. As a general rule, it is
    best to define our arguments in order of least variation to 
    the greatest variation.
-}
{-
    Wholemeal programming example
    
    Consider the function 'foobar' 
-}
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs
  
{-
    This function is not considered good Haskell style. It 
        (a) is doing to much at once
        (b) is working at too low of a level (at element level)
        
    Rather than thinking about what we want to do to each element,
    we need to start thinking in terms of how we want to transform
    to the entire input value.
    
    Below is a more idiomatic definition which chains together
    a series of steps; beginning at the right:
        apply a filter to grab all elements > 3
        multiply each remaining element by 7 and add 2
        sum the result
        
    'map' and 'filter' are both 'partial applications'
    This style of coding, in which we define a function without
    respect to its arguments, is called 'point-free' style [because
    the function does not apply to any specific point in its
    domain space but all such points]
-}  
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

{-
    Folds
    -----
    Below are three list functions that follow a similar pattern;
    they all combine a list into a single value

-}
sum' :: [Integer] -> Integer
sum' []     = 0                     -- base case
sum' (x:xs) = x + sum' xs           -- recursive case

product' :: [Integer] -> Integer
product' []     = 1                 -- base case
product' (x:xs) = x * product' xs   -- recursive case

length' :: [a] -> Int
length' []     = 0                  -- base case
length' (_:xs) = 1 + length' xs     -- recursive case

-- abstracted pattern
--  fold f z [a,b,c] == a `f` (b `f` (c `f` z))
fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z                     -- base case
fold z f (x:xs) = f x (fold z f xs)     -- recursive case

-- original functions, now using fold
---    fold base case operation
sum''     = fold 0 (+)
product'' = fold 1 (*)
length''  = fold 0 (\_ s -> 1 + s)  --  also (\_ -> (1+)) or (const (1+))

{-
    'fold' is defined in the Prelude as 'foldr'
    
    Some Prelude functions implemented using foldr:
    
    
    length  :: [a] -> Int
    sum     :: Num a => [a] -> a
    product :: Num a => [a] -> a
    and     :: [Bool] -> Bool
    or      :: [Bool] -> Bool
    any     :: (a -> Bool) -> [a] -> Bool
    all     :: (a -> Bool) -> [a] -> Bool

    There is also a 'foldl' which folds from the left
    
        foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c
        
    In practice, use foldl' - more efficient version of foldl
    

-}


