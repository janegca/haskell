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
    deriving Show
                    
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
evaluate (C x)      = x
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

-- how does this work??
--  each Leaf evaluates to 0 as 
--      init x -> (const 0) x -> 0
--  and each time a Branch is encountered, the lambda returns
--  a 1 plus the accumulated value of the times other branches
--  have been encountered
{-
      treeHeight' t1
    = foldTree (\x y -> (max x y) + 1) (const 0) t1
    
      -- pattern match on Branch
    = foldTree (\x y -> (max x y) + 1) 0 (Leaf 1)
      (\x y -> (max x y) + 1)
      foldTree (\x y -> (max x y) + 1) 0 (Branch (Leaf 2) (Leaf 3))
      
    = (const 0) 1                   -- match on (Leaf x)
      (\x y -> (max x y) + 1)
        -- pattern match on (Branch t1 t2)
        foldTree (\x y -> (max x y) + 1) 0 (Leaf 2)  
        (\x y -> (max x y) + 1)
        foldTree (\x y -> (max x y) + 1) 0 (Leaf 3)
        
    = 0                             -- value of a Leaf
      (\x y -> (max x y) + 1)
        (const 0) 2                 -- match on (Leaf x)
        (\x y -> (max x y) + 1)
        (const 0) 3                 -- match on (Leaf x)
        
    = 0
      (\x y -> (max x y) + 1)
        0 (\x y -> (max x y) + 1) 0 -- leaves evaluate to 0
        
    = 0
      (\x y -> (max x y) + 1)
        (\0 0 -> (max x y) + 1)     -- apply lambda
        
    = 0 (\x y -> (max x y) + 1) 1   -- evaluate lambda
    = (\0 1 -> (max 0 1) + 1)       -- apply lambda
    = 1 + 1                         -- evaluate
    = 2                             -- arithmetic
      
-}
treeHeight' = foldTree (\x y -> (max x y) + 1) (const 0)

{-
    Exercise 7.2
    
        Using the definition of InternalTree, define
        
        takeTree      :: Int -> InternalTree a -> InternalTree a
        takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
        
        The function 'takeTree' returns the first n levels of a tree,
        'takeWhile' should behave analogously.
        
    (solution ref: http://www.elbeno.com/haskell_soe_blog/?p=26)
-}
it1 :: InternalTree Int
it1 = let t' = IBranch 1 ILeaf ILeaf
      in IBranch 2 t' t'
      
takeTree :: Int -> InternalTree a -> InternalTree a
takeTree 0 _                 = ILeaf
takeTree _ ILeaf             = ILeaf
takeTree n (IBranch x t1 t2) = 
    IBranch x (takeTree (n-1) t1) (takeTree (n-1) t2)
    
takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile _ ILeaf = ILeaf
takeTreeWhile p (IBranch x t1 t2) 
    | p x       = IBranch x (takeTreeWhile p t1) (takeTreeWhile p t2)
    | otherwise = ILeaf
    
{-
    Exercise 7.3
        Using the InternalTree data type, define tree versions of
        the functions 'foldr' and 'repeat'  
        
    Standard implementation of 'foldr'
    
        foldr :: (a -> a -> a) -> a -> [a] -> a
        foldr f a [] = a
        foldr f a (x:xs) = f x (foldr f a xs)    
        
    Standard definition of 'repeat'; use with 'take' to avoid
    infinite lists
    
        repeat :: a -> [a]  
        repeat x = x : repeat x    

    {solution ref: http://www.elbeno.com/haskell_soe_blog/?p=27)
    
-}    
foldITree :: (a -> b -> b)          -- function to apply to tree values
          -> b                      -- base (initial) value
          -> InternalTree a         -- tree to fold
          -> b                      -- result
foldITree f init ILeaf             = init
foldITree f init (IBranch x t1 t2) = 
    foldITree f (f x (foldITree f init t1)) t2

ex73  = foldITree (+) 0 it1    -- 4
ex73a = foldITree (*) 1 it1    -- 2

-- use with takeTree
-- if an ILeaf is passed as the repeat all you get back is an ILeaf,
-- (you can't have a tree without branches)
-- if an IBranch is passed in, the same branch gets repeated
-- i.e. the 'value' of each IBranch is always the one in the original
--      IBranch
repeatTree :: InternalTree a -> InternalTree a
repeatTree    ILeaf          = ILeaf
repeatTree t@(IBranch x _ _) = IBranch x (repeatTree t)
                                         (repeatTree t)
                                             
ex73b = takeTree 0 (repeatTree (IBranch 1 ILeaf ILeaf))   
ex73c = takeTree 1 (repeatTree (IBranch 1 ILeaf ILeaf)) 
ex73d = takeTree 2 (repeatTree (IBranch 1 ILeaf ILeaf))
ex73e = takeTree 2 (repeatTree (IBranch "John" ILeaf ILeaf))

{-
    Output:
    
        *Main> ex73b
        ILeaf
        *Main> ex73c
        IBranch 1 ILeaf ILeaf
        *Main> ex73d
        IBranch 1 (IBranch 1 ILeaf ILeaf) (IBranch 1 ILeaf ILeaf)
        *Main> ex73e
        IBranch "John" (IBranch "John" ILeaf ILeaf) 
                       (IBranch "John" ILeaf ILeaf)
        *Main>

-}
{-
    Exercise 7.4
        Using either of the tree definitions, define tree versions
        of 'zip' and 'zipWith'
        
    Standard definition of 'zip':
        zip :: [a] -> [b] -> [(a,b)]
        zip (a:as) (b:bs) = (a,b) : zip as bs
        zip _      _      = []
        
    Standard definition of 'zipWith':
        zip :: (a -> b -> c)  -> [a] -> [b] -> [c]
        zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
        zipWith _  _      _     = []
        
    So, how do we combine trees? 
    
    Assume, for 'zip', that the values in each node become a tuple of 
    the values in the two zipped trees.
    
    Assume, for 'zipWith', that the values in the nodes become the value
    from applying the given function to the values of the
    corresponding nodes.
    
    Do we need to make sure the trees are the same size? height?
    
    The 'zip' and 'zipWith' functions truncate to the shortest
    passed in list; so we do the same for zipWith, if one tree runs out, 
    we stop; returning the last leaf in the shortest tree.  Cannot
    figure out how to do the same with 'zipTree'; either need to
    use Maybe for the result tree, throw an error or a new Null
    constructor to Tree.
    
    So, taking two example trees:
    
        t1 = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
        t2 = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) 
                    (Branch (Leaf 4) (Leaf 5))

        B              B
       / \            / \
      L1  B          B    B
         / \        / \  / \
        L2 L3      L1  B L4 L5
                      / \
                     L2 L3
    
-} 
-- zipTreeWith was the easiest to deal with
zipTreeWith :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
zipTreeWith f (Leaf x)     (Branch _ _)   = Leaf x
zipTreeWith f (Branch _ _) (Leaf y)       = Leaf y
zipTreeWith f (Leaf x) (Leaf y)           = Leaf $ f x y
zipTreeWith f (Branch a1 a2) (Branch b1 b2) = 
    Branch (zipTreeWith f a1 b1) (zipTreeWith f a2 b2)

    
-- zipTree, taking a number of different approaches

-- throwing an error if trees are different shapes    
zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree (Leaf a) (Leaf b)             = Leaf (a,b)
zipTree (Branch a1 a2) (Branch b1 b2) =
    Branch (zipTree a1 b1) (zipTree a2 b2)
zipTree _ _ = error "Trees must be same size and height."

-- using a Maybe to handle different size trees
zipTree' :: Tree a -> Tree b -> Tree (Maybe a,Maybe b)
zipTree' (Leaf a) (Leaf b)             = Leaf (Just a, Just b)
zipTree' (Branch a1 a2) (Branch b1 b2) =
    Branch (zipTree' a1 b1) (zipTree' a2 b2)
zipTree' _ _                           = Leaf (Nothing, Nothing)    
    
-- the third option is to add a 'Null' constructor to Tree
data NTree a = Null | NLeaf a | NBranch (NTree a) (NTree a)
    deriving Show

zipNTree :: NTree a -> NTree b -> NTree (a,b)
zipNTree (NLeaf a) (NLeaf b)             = NLeaf (a,b)
zipNTree (NBranch a1 a2) (NBranch b1 b2) =
    NBranch (zipNTree a1 b1) (zipNTree a2 b2)
zipNTree _ _ = Null

nt1 = NBranch (NLeaf 1) (NBranch (NLeaf 2) (NLeaf 3))
nt2 = NBranch (NBranch (NLeaf 1) (NBranch (NLeaf 2) (NLeaf 3)))
              (NBranch (NLeaf 4) (NLeaf 5))

-- Example Tests              
ex74a = zipTreeWith (+) t1 t1
ex74b = zipTreeWith (+) t1 t2
ex74c = zipTreeWith (*) t2 t2

ex74d = zipTree t1 t1      -- fine for same size trees
ex74e = zipTree t2 t2
ex74f = zipTree t1 t2      -- error if different size trees
ex74g = zipTree' t1 t2     -- using Maybe to handle diff size trees
              
ex74h = zipNTree nt1 nt1   -- add a Null ctor to handle diff size trees
ex74i = zipNTree nt2 nt2
ex74j = zipNTree nt1 nt2
     
{-
    Output:
    *Main> t1
    Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
    *Main> ex74a
    Branch (Leaf 2) (Branch (Leaf 4) (Leaf 6))
    
    *Main> t2
    Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) 
           (Branch (Leaf 4) (Leaf 5))
    *Main> ex74b
    Branch (Leaf 1) (Branch (Leaf 6) (Leaf 8))
    
    *Main> ex74c
    Branch (Branch (Leaf 1) (Branch (Leaf 4) (Leaf 9))) 
           (Branch (Leaf 16) (Leaf 25))
    *Main>    

    *Main> ex74d
    Branch (Leaf (1,1)) (Branch (Leaf (2,2)) (Leaf (3,3)))
    *Main> ex74e
    Branch (Branch (Leaf (1,1)) (Branch (Leaf (2,2)) (Leaf (3,3)))) (Branch (Leaf (4,4)) (Leaf (5,5)))
    *Main> ex74f
    *** Exception: Trees must be same size and height.
    Branch *Main> ex74g
    Branch (Leaf (Nothing,Nothing)) (Branch (Leaf (Just 2,Just 4)) (Leaf (Just 3,Just 5)))
    
    *Main> ex74h
    NBranch (NLeaf (1,1)) (NBranch (NLeaf (2,2)) (NLeaf (3,3)))
    *Main> ex74i
    NBranch (NBranch (NLeaf (1,1)) (NBranch (NLeaf (2,2)) (NLeaf (3,3)))) (NBranch (NLeaf (4,4)) (NLeaf (5,5)))
    *Main> ex74j
    NBranch Null (NBranch (NLeaf (2,4)) (NLeaf (3,5)))
    *Main>     
-}     
