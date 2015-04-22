{-# LANGUAGE NoImplicitPrelude, KindSignatures #-}
module Monads where
{-
    CIS 552: Advanced Programming (2015)
    Lecture 7 - Monads
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/stub/Monads.html
-}
import Prelude hiding (filter,(>>))
import Data.Char (toUpper)
import Control.Monad (guard)

{-
    Quiz
    ----
        Consider the definition of a tree with values stored
        in the leaves.
        
        Define a function, zipTree, that combines the two trees
        as follows:
        
        o                     o                      o
       / \                   / \                   /   \
     "a"  o        ===>     0   o        ===>  ("a",0)  o
         / \                   / \                     / \
       "b" "c"                1   2             ("b",1)  ("c", 2)        
-}
data Tree a = Leaf a | Branch (Tree a) (Tree a)
   deriving (Eq, Show)
   
-- | zip two trees together
zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree (Leaf a) (Leaf b) = Leaf (a,b)
zipTree (Branch a1 a2) (Branch b1 b2) = 
    Branch (zipTree a1 b1) (zipTree a2 b2)

testZip :: Bool
testZip = 
    zipTree (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
          (Branch (Leaf 0  ) (Branch (Leaf 1  ) (Leaf 2  )))
    ==
    (Branch (Leaf ("a",0)) (Branch (Leaf ("b",1)) (Leaf ("c",2))))   
    
{-
    Error Recovery (An example in abstraction)
    --------------
    Look at one way we could define zipTree so we could recover
    from an error:
    
        zipTree :: Tree a -> Tree b -> Maybe (Tree (a,b))
        zipTree (Leaf a) (Leaf b) = 
            Just (Leaf (a,b))   <-------------------- this is how we    
        zipTree (Branch l r) (Branch l' r') =          return a value
           ~~~~~~~~~~~~~~~~~~~~~~                              |
           case zipTree l l' of                                |
             Nothing -> Nothing                                |
             Just l'' ->           <------- This is how we     |
           ~~~~~~~~~~~~~~~~~~~~             use a value        |
               case zipTree r r' of         |                  |
                 Nothing -> Nothing   <-----|                  |
                 Just r'' ->                                   |
                ~~~~~~~~~~~~~~~~~~~~~~~                        |
                    Just (Branch l'' r'')   <------------------|    
        zipTree _ _ = Nothing

    Extracting the common parts of a defintion, we get:
    
        For 'returning' a value
            Just (Leaf (a,b))
            Just x
        which abstracts to:
            Just x
        for which we can create a function:
            return :: a -> Maybe a
            return x = Just x
            
        For 'using' a value we get:
        
            case zipTree l l' of
                Nothing  -> Nothing
                Just l'' -> ...
                    do somethin with l''
                    
            case zipTree r r' of
                Nothing  -> Nothing
                Just r'' -> ...
                    do something with r''
                    
        which abstracts to:
        
            case x of
                Nothing -> Nothing
                Just y  -> f y
                
        for which we can create a function 'bind':
            
            (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
            x >>= f = case x of
                        Nothing -> Nothing
                        Just y  -> f y
                        
    Use the new general definitions to refactor the code:

        zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
        zipTree2 (Leaf a) (Leaf b) = undefined
        zipTree2 (Branch l r) (Branch l' r') = undefined
        zipTree2 _ _ = Nothing    
                                
-}    
-- NOTE: the 'return' function wraps the results in a 'Just'
zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree2 (Leaf a) (Leaf b) = return $ Leaf (a,b) 
zipTree2 (Branch l r) (Branch l' r') =
    zipTree2 l l' >>= \l'' ->
    zipTree2 r r' >>= \r'' ->
    return $ Branch l'' r''     
zipTree2 _ _ = Nothing    

{-
    Haskell provides a special notation (syntactic sugar) for
    the (>>=) function:
    
        do x1 <- m1
           x2 <- m2
           ...
           xn <- mn
           f x1 x2 ... xn   
            
    So we could re-write zipTree2 as follows
-}
zipTree3 :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree3 (Leaf a) (Leaf b) = return $ Leaf (a,b)
zipTree3 (Branch l r) (Branch l' r') = 
   do l'' <- zipTree3 l l'  
      r'' <- zipTree3 r r'  
      return $ Branch l'' r''
zipTree3 _ _ = Nothing

{-
    Monads
    ------
    A monad (in Haskell) is a paramertized type, m, combined with two
    functions:
    
        class Monad m where
          return :: a -> m a
          (>>=)  :: m a -> (a -> m b) -> m b   
          
          (>>)  :: Monad m => m a -> m b -> m b
           m1 >> m2 = m1 >>= \_ -> m2

    To add a type to the Monad class, create an instance that
    implements the two functions. For example, the the Monad Maybe
    instance is defined as:
    
        instance Monad Maybe where
           -- return      :: a -> Maybe a
           return x       =  Just x

           -- (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
           Nothing  >>= _ =  Nothing
           (Just x) >>= f =  f x    

    Note that type signatures are not permitted in instance
    declarations; added here as comments.
    
    The 'do' notation will work with any Monad; the Haskell 
    compiler converts it to a sequence of binds; for example,
    
        do x1 <- m1
           x2 <- m2
           ...
           xn <- mn
           f x1 x2 ... xn    
           
    becomes

        m1 >>= (\x1 ->
          m2 >>= (\x2 ->
             ...
              mn >>= (\xn ->
                f x1 x2 ... xn)...))   

    When we don't want to use the result of a computation, i.e.
                
       main = do 
          x <- doSomething
          doSomethingElse     -- what is going on here?
          y <- andSoOn
          f x y
    
    we don't assign it a variable and underneath the hood, the
    compiler converts it using (>>) instead of (>>=); (>>)
    acts the same as (>>=) except it ignores a function result
    rather than passing it onto the next function in the sequence
    
        m1 >>= \ _ -> m2
-}
(>>)  :: Monad m => m a -> m b -> m b
m1 >> m2 = m1 >>= \_ -> m2

whatDoesThisDo :: IO ()
whatDoesThisDo = foldr (>>) (putStrLn " Batman!")
           (replicate 10 (putStr (show (0.0/0.0))))
    -- NaNNaNNaNNaNNaNNaNNaNNaNNaNNaN Batman!

{-
    While the Maybe monad provides a computational model for things
    that can fail, the List monad generalizes the idea of multiple
    successes [x1,x2,x3...] or one failure, [].
-}    
pairs :: [Int] -> [Int] -> [(Int,Int)]
pairs xs ys = concatMap (\x -> concatMap (\y -> [(x,y)]) ys) xs

testPairs = pairs [1,2,3,4] [5,6,7,8]

{-
    The patterns here are:
        concatMap (\x ->  do something with x) xs
        concatMap (\y ->  do something with y) ys         

    which generalize to:
        concatMap f xs
        
    which can be re-jiggered to:
    
        (>>=) :: [a] -> (a -> [b]) -> [b]
         xs >>= f = concatMap f xs    

    and return would be
    
        return :: a -> [a]
        return x = [x]

    so we can rewrite pairs as:
-}    
pairs1 :: [Int] -> [Int] -> [(Int,Int)]
pairs1 xs ys = xs >>= \x -> ys >>= \y -> return (x,y)

testPairs1 = pairs1 [1,2,3,4] [5,6,7,8] == pairs [1,2,3,4] [5,6,7,8]

-- or we can rewrite it using do-notation
pairs2 :: [Int] -> [Int] -> [(Int,Int)]
pairs2 xs ys = do x <- xs
                  y <- ys
                  return (x,y)
                 
testPairs2 = pairs2 [1,2,3,4] [5,6,7,8] == pairs1 [1,2,3,4] [5,6,7,8]

{-
    The list monadic instance is straightforward:
    
        instance Monad [] where
           -- return :: a -> [a]
           return x  =  [x]
                           
           -- (>>=)  :: [a] -> (a -> [b]) -> [b]
           xs >>= f  =  concatMap f xs    

-}              
{-
    The notation used in defining 'pairs2' is very similar
    to list comprehension notation

-}
pairs3 :: [Int] -> [Int] -> [(Int,Int)]
pairs3 xs ys =  [(x,y)|x <- xs, y <- ys]

testPairs3 = pairs2 [1,2,3,4] [5,6,7,8]   == pairs3 [1,2,3,4][5,6,7,8]

-- rewrite the map funciton using list comprehension
map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs ]

testMap = map' (+2) [1,2,3] == map (+2) [1,2,3]
     
-- create a list of all pairs where the first component is less
-- than the second
pairs4 :: [Int] -> [Int] -> [(Int,Int)]
pairs4 xs ys = [(x,y) | x <- xs, y <- ys, x < y ]

testPairs4 = pairs4 [1..10][1..10]

-- rewrite it using do notation
pairs4' :: [Int] -> [Int] -> [(Int,Int)]
pairs4' xs ys = do x <- xs
                   y <- ys
                   if x < y then return (x,y) else []
                  
testPairs4' = pairs4' [1..10][1..10] == pairs4 [1..10][1..10]

-- can be rewritten using 'guard' (part of Monad??)
pairs4'' :: [Int] -> [Int] -> [(Int,Int)]
pairs4'' xs ys = do x <- xs
                    y <- ys
                    guard (x < y)
                    return (x,y)

testPairs4'' = pairs4'' [1..10][1..10] == pairs4 [1..10][1..10]

-- rewrite filter using a guarded list comprehension
filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [ x | x <- xs, f x ]

testFilter = filter odd [1..10]

-- rewriting quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<= x) xs)
                ++ [x]
                ++ quicksort (filter (> x) xs)  

testQsort = quicksort [3,2,9,1,0]                

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' [ a | a <- xs, a <= x]
                ++ [x]
                ++ quicksort' [ b | b <- xs, b > x ]            
                    
testQsort' = quicksort' [3,2,9,1,0] == quicksort [3,2,9,1,0]
           
           