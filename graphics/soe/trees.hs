{-  
    Chapter 7 - Trees

    Reference:
        'The Haskell School of Expression' by Paul Hudak
        http://www.cs.yale.edu/homes/hudak/SOE/index.htm
-}

-- place elements in the leaves of the tree
data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving Show

-- place elements on the internal branches
data InternalTree a = ILeaf
                    | IBranch a (InternalTree a) (InternalTree a)
                    
-- place elements on both leaves and branches
-- elements can have different types
data FancyTree a b = FLeaf a
                   | FBranch b (FancyTree a b) (FancyTree a b)
    deriving Show

exFT = FBranch 42 (FLeaf True) (FLeaf False)
    
-- a map function for our basic tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x)       = Leaf (f x)
mapTree f (Branch t1 t2) = Branch (mapTree f t1)
                                  (mapTree f t2)

-- combine all the leaves on a tree and return them as a list
fringe :: Tree a -> [a]
fringe (Leaf x)       = [x]
fringe (Branch t1 t2) = fringe t1 ++ fringe t2
            
-- compute the size (number of leaves) in a tree
treeSize :: Tree a -> Integer
treeSize (Leaf x)       = 1
treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

-- compute tree height (leaves have a height of 0)
treeHeight :: Tree a -> Integer
treeHeight (Leaf x) = 0
treeHeight (Branch t1 t2) = 1 + max(treeHeight t1)
                                   (treeHeight t2)
                                   
-- building arithmetic expressions as trees
data Expr = C Float             -- constant
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
    deriving Show

-- represent (10 + 8/2) * (7 - 4)          
exExpr = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4) 

evaluate :: Expr -> Float
evaluate (C x)  = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2

exEval = evaluate exExpr        -- 42.0
         
-- Exercises ------------------------------------------------------------
--
-- Exercise 7.1
--      Design a higher-order function that captures the common pattern
--      of recursion and redefine 'fringe' and 'treeSize' in terms of it
--
--  (solution ref: http://www.elbeno.com/haskell_soe_blog/?p=25) )
--
t1 = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
t2 = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) 
            (Branch (Leaf 4) (Leaf 5))

foldTree :: (b -> b -> b)   -- operation to perform on elements
         -> (a -> b)        -- initial value as a function
         -> Tree a          -- tree to be folded
         -> b               -- result
foldTree op init (Leaf x)       = init x
foldTree op init (Branch t1 t2) =
    (foldTree op init t1) `op` (foldTree op init t2)
    
fringe'     = foldTree (++) (:[])
treeSize'   = foldTree (+)  (const 1)

-- TODO: a walkthrough on this, why does it work??
treeHeight' = foldTree (\x y -> max x y+1) (const 0)

