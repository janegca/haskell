{-# LANGUAGE NoImplicitPrelude #-}

{-
    CIS 552: Advanced Programming (2015)
    Lecture 8 - Monads Part 2
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/stub/Monads2.html
-}

module Monads2 where

import Prelude hiding (getLine,sequence,(>>))
import Data.Map (Map)   -- access to constructors without qualification
import qualified Data.Map as M
import System.Random (StdGen, next, split, mkStdGen)

import State    -- simple version of Control.Monad.State

{-
    We look first at an awkward way of writing functions that
    manipulate 'state' and then a how the State monad makes
    things easier.
-}
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)
  
-- example Tree  
tree :: Tree Char
tree =  Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- counting the leaves in a tree
countF :: Tree a -> Int
countF (Leaf _)       = 1
countF (Branch t1 t2) = countF t1 + countF t2

-- we can use the 'state transformer' pattern to emulate incrementing
-- a variable (as in an imperative language like C or Java)

type Store = Int        -- the 'State'

countI :: Tree a -> Int
countI t = aux t 0 where
  aux :: Tree a -> Store -> Store
  aux (Leaf _) s       = s + 1
  aux (Branch t1 t2) s = aux t1 (aux t2 s)
 
-- in general, a state transformer takes a current state and returns
-- a modified state, with the modified state reflecting any effects
-- performed by the function

-- consider a function that labels each leaf with a new integer
-- where we return a result value as well as the store
label1 :: Tree a -> Tree (a, Int)
label1 t = fst (aux t 0) where
   aux :: Tree a -> Store -> (Tree(a,Int), Store)
   aux (Leaf x) s       = (Leaf (x,s) , s+1)
   aux (Branch t1 t2) s = let (t1',s')  = aux t1 s  in
                          let (t2',s'') = aux t2 s' in
                          (Branch t1' t2', s'')
                          
{- generalizing the return of a value and a store we get the following 
   type                       
        type ST a = Store -> (a, Store)   

    which we can make part of the Monad class
        instance Monad ST where
           --       return :: a -> Store -> (a, Store)
           -- i.e., return :: a -> ST a
           -- turns a value into a state transformer
           return x  =  \st -> (x,st)  -- same as return x s = (x,s)

           -- (>>=)  :: (Store -> (a, Store)) ->
           --           (a -> (Store -> (b, Store))) -> 
           --            (Store -> (b, Store))
           -- provide a means of sequencing state transformers
           st >>= f  =  \s -> let (x, s') = st s in f x s'

    Technical detail re: type and instance
        a 'type' cannot be made into an 'instance', in reality ST
        would have to be a 'data' or 'newtype' which requires a
        dummy constructor, S
        
        [Note: trying to load above definitions in ghci resulted
               in 'type synonym' error
        
        We can remove the need for the dummy constructor by 
        defining our own application type function
-}
newtype ST2 a = S {apply :: Store -> (a, Store)}

{-
    *Monads2> :t S
    S :: (Store -> (a, Store)) -> ST2 a
    *Monads2> :t apply
    apply :: ST2 a -> Store -> (a, Store)
    *Monads2> 
-}

-- defining ST2 as a Monadic type
instance Monad ST2 where
  -- return :: a -> ST2 a
  return x   = S (\ s -> (x,s))

  -- (>>=)  :: ST2 a -> (a -> ST2 b) -> ST2 b
  st >>= f   = S (\s -> let (x,s') = apply st s 
                        in apply (f x) s')

-- Technical note: there is no runtime overhead for manipulating the
--                 constructor 'S' as we used 'newtype' instead of 'data'

-- We can now rewrite the tree labelling function using the State
-- transformer; we get a new number by incrementing the store
fresh :: ST2 Int  -- Store -> (Int, Store)
fresh = S (\ s -> (s , s + 1))

mlabel :: Tree a -> ST2 (Tree (a,Int))
mlabel (Leaf x)     = -- fresh >>= \v -> return (Leaf (x, v))
                     do v <- fresh
                        return (Leaf (x,v)) 
mlabel (Branch l r) = do l' <- mlabel l    -- :: ST2 (Tree (a,Int))
                         r' <- mlabel r    -- :: ST2 (Tree (a,Int))
                         return (Branch l' r')
                         
-- label a tree
label  :: Tree a -> Tree (a, Int)
label t = fst $ apply (mlabel t) 0

tstLabel = label tree
      
-- Exercises
-- Define a function app :: (Store -> Store) -> ST2 Store, such that 
-- fresh can be redefined by fresh = app (+1)
app :: (Store -> Store) -> ST2 Store  
app f = S (\ s -> (s, f s))

fresh' :: ST2 Store
fresh' = app (+1)

-- Define a function run :: ST2 a -> Store -> a, such that label can be 
-- redefined by label t = run (mlabel t) 0
run :: ST2 a -> Store -> a
run m s = fst $ apply m s
  
label' :: Tree a -> Tree (a, Int)  
label' t = run (mlabel t) 0

{-
    A Generic State transformer allows our 'store' to be any
    type.
    
    We can use the generic state transformer in State.lhs to
    rewrite labelling function.

-}
freshS :: State Int Int
freshS = do x <- get
            put (x + 1)
            return x
            
mlabelS :: Tree t -> State Int (Tree (t,Int))
mlabelS (Leaf x) = do s <- freshS
                      return $ Leaf (x,s)
mlabelS (Branch l r) = do l' <- mlabelS l
                          r' <- mlabelS r
                          return $ Branch l' r'

testMLS = runState (mlabelS tree) 0
    -- (Branch (Branch (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2)),3)
testMLS' = runState (mlabelS tree) 1000
    -- (Branch (Branch (Leaf ('a',1000)) (Leaf ('b',1001))) 
    --                 (Leaf ('c',1002)),1003)
{-
    Next, we can extend fresh and label so:
        - each node gets a new label
        - the state contains a map of the 'frequency' with which
          each leaf value appears in the tree
          
    Ref: https://github.com/logicshan/jekyll_demo/miscellanea/monad2-2.hs
-}
data MySt a = M { index :: Int
                , freq  :: Map a Int }
    deriving (Eq, Show)
              
freshM :: State (MySt a) Int
freshM = do x <- get
            let n = index x             -- record in MySt
            put $ x { index = n + 1 }
            return n
            
updFreqM :: Ord a => a -> State (MySt a) ()
updFreqM k = do s <- get
                let f = freq s                        -- record in MySt
                let n = M.findWithDefault 0 k f
                put $ s {freq = M.insert k (n + 1) f}
                
mlabelM :: Ord a => Tree a -> State (MySt a) (Tree (a, Int))
mlabelM (Leaf x)     =  do i <- freshM
                           updFreqM x
                           return $ Leaf (x,i)
mlabelM (Branch l r) =  do l' <- mlabelM l
                           r' <- mlabelM r
                           return $ Branch l' r'

-- our initial state                           
initM :: MySt a
initM = M 0 M.empty                           
                
tree2 = Branch tree tree
(lt, s) = runState (mlabelM tree2) initM
            
{-
    *Monads2> lt
    Branch (Branch (Branch (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))) (Branch (Branch (Leaf ('a',3)) (Leaf ('b',4))) (Leaf ('c',5)))
    *Monads2> s
    M {index = 6, freq = fromList [('a',2),('b',2),('c',2)]}
    *Monads2> 

-}   
  