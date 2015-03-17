module W07 where

import Data.Monoid

{-
    Week 07 - Folds and Monoids - Notes
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-} 
{-
    Folds, again
    ------------
    We can apply folds to other data structures
    
    Look at all the functions over trees given below: 
        treeSize, treeDepth, treeSum, flatten
        
    They all follow the same pattern, they:
    
        1. take a Tree as input
        2. pattern-match on the Tree input
        3. give a simple answer for the Empty case
        4. do two things for the Node case:
            a. call themselves recursively
            b. combine the results of the calls with data x
               to produce a final result
               
    To generalize this pattern we will need to isolate the
    changing elements so they can be passed as parameters.
    The parts of the example functions that change are:
    
        1. the return type
        2. the answer to the Empty case
        3. the manner in which the recursive calls are combined
        
    This can be abstracted into the 'treeFold' function which
    will allow us to rewrite all the previous functions.
    
    We can do something similar over ExprT. In fact, we can
    implement folds for many (but not all) data structures.
    
    "The fold for T will take one (higher-order) argument for each 
     of T’s constructors, encoding how to turn the values stored by
     that constructor into a value of the result type—assuming that
     any recursive occurrences of T have already been folded into a
     result."

-}
-- define a Tree data structure
data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)
  
-- define a smart constructor
leaf :: a -> Tree a
leaf x = Node Empty x Empty  

-- compute the size of a tree
treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

-- compute the sum of integers in a Tree Integer structure
treeSum :: Tree Integer -> Integer
treeSum Empty         = 0
treeSum (Node l x r)  = x + treeSum l + treeSum r

-- compute the depth of a tree
treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

-- convert (flatten) a tree into a list
flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

-- treeFold also makes it easier to write new functions over trees
treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)

-- expression data structure
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

-- count the number of literals in an expression
numLiterals :: ExprT -> Int
numLiterals = exprTFold (const 1) (+) (+)

{-
    Monoids
    -------
    Another standard type class we should know about is:
    
            class Monoid m where
            mempty  :: m
            mappend :: m -> m -> m

            mconcat :: [m] -> m
            mconcat = foldr mappend mempty

            (<>) :: Monoid m => m -> m -> m
            (<>) = mappend
            
    'mempty' is the identity function
    (<>) is associative and takes two values and combines them into 
         a third
         
    A monoid has to fullfill three rules:
    
            mempty <> x      == x
            x      <> mempty == x
            (x <> Y) <> z    == x <> (y <> z)
            
    The class also supplies 'mconcat' which combines all the 
    a whole list of values; there is a default implementation
    which uses foldr but it can be overridden as necessary.
    
    Data.Monoid provides a number Monoids:
        Sum, Product, Any, All
    Sum and Product wrap the integer (+) and (*) operations
    Any and All wrap the logical boolean operators (||) and (&&)
    
    Pairs form a monoid as long as the individual components
    are also monoids.
-}
-- example to find the product of a list of integers using mconcat
-- we must first turn them into elements type Product Integer
lst :: [Integer]
lst = [1,5,8,23,423,99]

prod :: Integer
prod = getProduct . mconcat . map Product $ lst

