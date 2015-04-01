{-
    Week 12 Monads
        Notes from reading 'The Trivial Monad'
        -- added Applicative instance
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/12-monads.html

    Reading: Typeclassopedia - recommends 'The Trivial Monad'
    http://blog.sigfpe.com/2007/04/trivial-monad.html
    
-}
import Control.Applicative

-- all this does is wrap a value
data W a = W a deriving Show

-- wrap anything we like
return' :: a -> W a
return' x = W x

-- manipulate wrapped data and keep it wrapped
fmap' :: (a -> b) -> (W a -> W b)
fmap' f (W x) = W (f x)

-- wrap a number and increment it
a = W 1
b = fmap' (+1) a

-- problem is, if we have a function like this
-- we can't apply it twice in a row to the same wrapped
-- value
f :: Int -> W Int
f x = W (x+1)

-- we need a way to unwrap the value, apply the function
-- and then re-wrap it
bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

c = bind f (f 1)                -- W 3

{-
        bind f (f 1)
     -> bind f (W (1 + 1))
     -> bind f (W 2)
     -> bind f 2
     -> bind W (2 + 1)
     -> bind W 3
   
-}

d = bind f (bind f (f 1))       -- W 4

-- notice that bind is very similar to fmap and, in fact,
-- fmap can be re-written using bind and return
--      fmap f = bind (return . f)

-- Exercise 1
g :: Int -> W Int -> W Int
g x y = bind (return' . (+x)) y

w4  = W 4
ex1 = g 3 w4     -- W 7

{-
        g 3 w4
     -> bind (return' . (+3)) (W 4)
     -> return' . (+3) 4
     -> return' . (4 + 3)
     -> return' 7
     -> W 7

-}

-- Exercise 2
h :: W Int -> W Int -> W Int
h x y = bind (flip g y) x

ex2 = h a w4        -- W 5

{-
        h a w4
     -> h (W 1) (W 4)
     -> bind (flip g (W 4)) (W 1)
     -> flip g (W 4) 1
     -> g 1 (W 4)
     -> bind (return' . (+1) (W 4)
     -> return' . (4 + 1)
     -> return' 5
     -> W 5
-}

-- with monads, the 'bind' is replaced by (>>=) so
-- bind f = x >>= f

-- Why bother wrapping data?
-- Wrapped data is 'tainted' data so results using it are 'tainted'

-- We can rewrite W as a Monad and add it to the Functor
-- and Applicative classes

data W' x = W' x deriving Show

instance Functor W' where
    fmap f (W' x) = W' (f x)
    
instance Applicative W' where
    pure          = W'
    W' f <*> W' x = W' (f x)
    
instance Monad W' where
    return x  = W' x
    W' x >>= f = f x
    
-- because a data constructor is just another function, 
-- it too can be mapped over values    
exApp =  W' <$> [1,2,3]     -- [W' 1,W' 2,W' 3]

h' :: (Applicative f, Num a) => f a -> f a -> f a
-- adding two wrapped values
h' x y = (+) <$> x <*> y

g' :: (Applicative f, Num a) => a -> f a -> f a
-- adding an unwrapped value to a wrapped value
g' x y = h' (pure x)  y

exApp1 = h' (W' 2) (W' 4)       -- W' 6
exApp2 = g' 2 (W' 3)            -- W' 5

-- My summary:
--      A Monad is just another type class
--      It is used to 'wrap' values and signals that any
--      such values are 'tainted' i.e. they are valid within
--      the specific context in which they arise but not
--      outside of it as they may rely on other elements within
--      that same context (??)
--
--      A Functor is a type class that provides a means
--      to map functions over (interact with) wrapped elements
--
--      Applicative is another type class that provides the
--      means to map Functors over wrapped elements
--
--      Point of the 3 classes to ensure 'tainted' operations
--      remain wrapped i.e. within the context they arose in
--
--  All appears to be directly related to 'data structures'
--  Monad's wrap values in specific data structures; structures
--  that are Functors provide a method for 'visiting' each
--  element in the structure. Structures that are Applicative
--  provide a means to 'visit' each element in multiple ways
--  i.e. say you have a dictionary of names and numbers stored
--       in a tree structure. You might want to extract all
--       names with numbers in a particular area code whose
--       last name begins with an 'S'.  Instead of having to
--       go through the tree twice, you can use the 
--       Applicative functions <$> and <*> to visit each
--       element once, check both the area code and first char
--       of the last name and return only those elements which
--       meet those requirements.





