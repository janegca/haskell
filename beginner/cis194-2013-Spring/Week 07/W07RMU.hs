{-# LANGUAGE FlexibleInstances #-}
module W07RMU where

import Data.Monoid
import Data.Foldable
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow

{-
    Week 07 - Folds and Monoids
              Notes from Suggested Readings for Monoids
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-}
{-
    Ref: Dan Piponi, Haskell Monoids and their Uses
         http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
         
    Defining Monoids
    ----------------
    A monoid, in Haskell, is a type that defines a rule describing
    how two elements may be combined to form a third element. The
    rule must also define what constitutes a null element for the type.
    For example, lists may be joined together using (++) and [].
    and Integers may be combined using (+) and 0.
    
    A possible definition for a Monoid class is:
    
        class Monoid m where
            mappend :: m -> m -> m   -- how to combine two elements
            mempty  :: m             -- what constitutes the null element
            
    We could then make an instance for lists:
    
        instance Monoid [a] where
        mappend = (++)
        mempty  = []
        
    Our monoids also need to obey a few rules, to ensure nothing
    happens with null elements:
    
        a      `mappend` mempty = a
        mempty `mappend` a      = a
        
    And, they must be associative, to ensure the order in which
    we combine elements doesn't matter:
    
        (a `mappend` b) `mappend` c == a `mappend` (b `mappend` c)
        
    Note that Haskell doesn't enforce these laws; it's up to you,
    the programmer, to make sure they hold.
-}
{-    
    Some Uses of Monoids
    --------------------
    Why use a monoid when we already have (++) and (+)? Because,
    with monoids, we get the concatenation function, mconcat,
    for free. For example, 
        
        mconcat [a,b,c] == a `mappend` (b `mappend` c)
        
    This 'free' behaviour is why we need our monoids to obey
    the associativity rule.
    
    Another reason to use a monoid is that it clearly signals
    the code's intent. If we see a function with the type
    signature (Monoid a) => a -> b, we immediately know what
    the function can do with with the data structure
    i.e. it may add elements (with mappend) but not remove any
    
    The same type can be turned into a monoid in many different
    ways. For example, an Integer can have varying definitions
    for mappend and mempty
    
        instance Monoid Integer where
        mappend = (+)
        mempty = 0
        
        instance Monoid Integer where
        mappend = (*)
        mempty = 1
        
    BUT, both of these cannot be instances at the same time. [You
    get compile errors for duplicate instances].  To get around
    this problem Data.Monoid defines wrappers for the integer
    functions (+) and (*):
    
        Num a => Monoid (Sum a)
        Num a => Monoid (Product a)
        
    We then create elements using these wrappers
    
-}
sum1  = getSum $ mconcat [Sum 2, Sum 3, Sum 4]                  -- 9
prod1 = getProduct $ mconcat [Product 2, Product 3, Product 4]  -- 24

{-
    The Writer Monad
    ----------------
    We can think of monoids as 'accumulators'; given a running total
    n, we can get a new total  n' = n `mappend` a.  Accumulating
    totals is a common design pattern so it's useful to have an
    abstraction to handle it; and this is what the Writer monad
    does, it accumulates values as a 'side effect'.
    
    The function that actually does this is called 'tell'. It's
    defined in Data.Monad.Writer as:
    
        tell :: MonadWriter w m => w -> m ()

    Below is an implementation of factorial that logs our progress
    using 'tell'; to see the results of fact1 we need to use
    'runWriter' which returns a tuple whose first element is the
    final value of the function and whose second element is the
    accumulated value.

-}
fact1 :: Integer -> Writer String Integer
fact1 0 = return 1
fact1 n = do
    let n' = n-1
    tell $ "We've taken one away from " ++ show n ++ "\n"
    m <- fact1 n'
    tell $ "We've called f " ++ show m ++ "\n"
    let r = n*m
    tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
    return r
    
fact1a = runWriter (fact1 10)
fact1b = fst fact1a             -- factorial (result of the function)
fact1c = putStrLn (snd fact1a)  -- steps taken (accumulator)
    
{-
    We can use the Writer Monad with any monoid. Below it is
    used to count the number of multiplications and subtractions
    needed to compute a given factorial.
-}    
fact2 :: Integer -> Writer (Sum Integer) Integer
fact2 0 = return 1
fact2 n = do
    let n' = n-1
    tell $ Sum 1
    m <- fact2 n'
    let r = n*m
    tell $ Sum 1
    return r
    
fact2a = runWriter (fact2 10)
fact2b = fst fact2a               -- factorial
fact2c = getSum $ (snd fact2a)    -- number of steps

{-
    We could have written the above (fact2) using the State
    Monad; using 'runState' to extract the value
-}
fact3 :: Integer -> State Integer Integer
fact3 0 = return 1
fact3 n = do
    let n' = n-1
    modify (+1)
    m <- fact3 n'
    let r = n*m
    modify (+1)
    return r
    
fact3a = runState (fact3 10) 0      -- (3628800,20)

{-
    fact3 works as well as fact2; however, there is an advantage
    to using the Writer vs the State Monad. From the type signature
    
        fact2 :: Integer -> Writer (Sum Integer) Integer
    
    we know, immediately, that the function has a side effect in
    which a number is accumulating in a positive way; nothing will
    ever bye multiplied, added, subtracted, etc.
    
    fact3, using the State monad, is free to do whatever it likes
    making it more difficult to discern its purpose.

-}
{-
    The Any and All Monoids
    -----------------------
    This is also defined in Data.Monoid and wraps the logical
    boolean 'or' operator (||).  It embodies the idea of setting
    a 'flag' which is switched on (True) or off (False) when
    some condition is met.
    
    fact4, below, provides an example of its use. Again, the
    function type signature describes what we may expect (a side
    effect that sets a flag).
    
    An 'All' monoid is also available, it wraps logical and (&&)

-}
fact4 :: Integer -> Writer Any Integer
fact4 0 = return 1
fact4 n = do
    let n' = n-1
    m <- fact4 n'
    let r = n*m
    tell (Any (r==120))     -- set the 'flag'
    return r
   
fact4a = runWriter (fact4 10)   -- (3628800,Any {getAny = True})
fact4b = fst fact4a             -- 3628800
fact4c = getAny $ (snd fact4a)  -- True

{-
    Commutative Monoids, Non-Commutative Monoids and Dual Monoids
    -------------------------------------------------------------
    A monoid is said to be 'commutative' if all its elements are
    interchangeable i.e. integers are commutative as a+b == b+a
    'flip mappend' produces the same result as mappend.
    
    A monoid type is non-commutative it the above is not true
    i.e. [1,2] ++ [3,4] /= [3,4] ++ [1,2]. So 'flip mappend'
    produces a different value than mappend.
    
    Data.Monoid provides another monad, Dual, which 'flips' the
    monoid arguments. We can use it to reverse the order of
    accumulation.
-}    
-- shows calculation steps in reverse order
fact5 :: Integer -> Writer (Dual String) Integer
fact5 0 = return 1
fact5 n = do
    let n' = n-1
    tell $ Dual $ "We've taken one away from " ++ show n ++ "\n"
    m <- fact5 n'
    tell $ Dual $ "We've called f " ++ show m ++ "\n"
    let r = n*m
    tell $ Dual $ "We've multiplied " ++ show n 
                                      ++ " and " ++ show m ++ "\n"
    return r
    
fact5a = runWriter (fact5 10)
fact5b = fst fact5a
fact5c = putStrLn $ getDual (snd fact5a)   

{-
    The Product Monoid
    ------------------
    If we want to accumulate two side-effects at the same time
    we can combine two monoids into one product monoid
    
        instance (Monoid a,Monoid b) => Monoid (a,b) where
        mempty = (mempty,mempty)
        mappend (u,v) (w,x) = (u `mappend` w,v `mappend` x)
        
    and create two helper functions to allow us to use two
    monoids simultaneously.
    
    The approach allows us perform multiple accumulations
    with ONE traversal of the data structure.

-}
fact6 :: Integer -> Writer (String,Sum Integer) Integer
fact6 0 = return 1
fact6 n = do
    let n' = n-1
    tellSnd (Sum 1)
    tellFst $ "We've taken one away from " ++ show n ++ "\n"
    m <- fact6 n'
    let r = n*m
    tellSnd (Sum 1)
    tellFst $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
    return r
    where
        tellFst a = tell $ (a,mempty)
        tellSnd b = tell $ (mempty,b)
        
fact6a = runWriter (fact6 5)
fact6b = fst fact6a             -- 120,  the function result
fact6c = snd fact6a             -- second value is another tuple
fact6d = putStrLn (fst fact6c)  -- trace of steps
fact6e = getSum $ (snd fact6c)  -- # of steps

{-
    Foldable Data
    -------------
    The Data.Foldable library provides a generic approach to
    walking through data structures, accumulating values as
    we go using 'foldmap' which applies a function to each
    element and accumulates the result of the applications.
    
    We can implement a tree structure and create add it to
    the Foldable type class, after which we can use any of
    the above monoids to traverse the tree.

-}
data Tree a = Empty 
            | Leaf a 
            | Node (Tree a) a (Tree a)

instance Foldable Tree where
    foldMap f Empty        = mempty
    foldMap f (Leaf x)     = f x
    foldMap f (Node l k r) = foldMap f l `mappend` f k 
                                         `mappend` foldMap f r
        
tree = Node (Leaf 1) 7 (Leaf 2)

ex7 = getAny $ foldMap (Any . (== 1)) tree
ex8 = getAll $ foldMap (All . (> 5))  tree

-- create min and max monoids along the lines of Any and All
data Smallest a = Smallest a    | PlusInfinity deriving (Show,Eq,Ord)
data Largest a  = MinusInfinity | Largest a deriving (Show,Eq,Ord)

instance (Ord a) => Monoid (Smallest a) where
    mempty  = PlusInfinity
    mappend = min

instance (Ord a) => Monoid (Largest a) where
    mempty = MinusInfinity
    mappend = max
    
ex10 = foldMap (Smallest &&& Largest) tree :: (Smallest Int,Largest Int)    
        