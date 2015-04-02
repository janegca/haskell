{-
    Week 12 Monads
        Notes from readings
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/
            lectures/12-monads.html

    Reading: Learn You a Haskell - A Fistful of Monads
        http://learnyouahaskell.com/a-fistful-of-monads 
    
-}
import Control.Applicative
import Control.Monad
import Data.Char

{-
    Monads are applicative functors that support (>>=) [bind]
    
    Monad definition
    ----------------
    
        class Monad m where  
            return :: a -> m a                   -- identity 
          
            (>>=) :: m a -> (a -> m b) -> m b    -- seq app with bind
          
            (>>) :: m a -> m b -> m b            -- sequential application
            x >> y = x >>= \_ -> y  
          
            fail :: String -> m a                -- we don't use this
            fail msg = error msg  

    Note that the class definition should technically read
        class (Applicative m) => Monad m where
        
    as all Monads ARE Applicative [just as all Applicatives are Functors]
    but no one had thought to add Applicative to Haskell before Monads 
    were added [?? why no (Functor m) constraint??]
    [in ghc v 7.10 all Monads will be explicitly made Applicative]  

    Monad Laws
    ----------
        Left Identity       return x >>= f  == f x
        Right Identity      m >>= return    == m
        Associativity       (m >>= f) >>= g == m >>= (\x -> f x >>= g)
        
    Monadic Composition Operator
    ----------------------------
    
    (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
    f <=< g = (\x -> g x >>= f)  
    
    Can re-write the laws as:
    
        f <=< (g <=< h)  == (f <=< g) <=< h
        f <=< return     == f
        return <=<       == f
    
-}
f x = [x, -x]
g x = [x*3, x*2]
h   = f <=< g
mc1 = h 3           -- [9,-9,6,-6]


{-    
    The Maybe Monad
    ---------------
    A value of type 'Maybe a' represents a normal value 'a' with 
    with the context of possible failure attached 
    e.g. Maybe String could have a string value or it could be
         Nothing (if the string is looked at as a the result
         of computation then Nothing means the computation failed)
         
    Looked at as a functor, if we want to fmap a function over it
    the function is applied to any value wrapped in Just but
    no function application is made to a Nothing since there is
    no 'value' to be obtained

-}
ex1 = fmap (++ "!") (Just "wisdom")         -- Just "wisdom!"
ex2 = fmap (++ "!") Nothing                 -- Nothing

{-
    Looked at as an applicative functor, both the function and
    the value need to be wrapped in Just if we want to use
    the functor application operator <*>
-}
ex3 = Just (+3) <*> Just 3              -- Just 6
ex4 = Nothing   <*> Just "greed"        -- Nothing
ex5 = Just ord  <*> Nothing             -- Nothing

{-
    If we want to apply a normal function, using <$> [fmap], 
    to Maybe values, again, all the values must be wrapped in Just

-}
ex6 = max <$> Just 3 <*> Just 6         -- Just 6
ex7 = max <$> Just 3 <*> Nothing        -- Nothing

{-
    So, how would we define (>>=) for Maybe?
    Remember that (>>=) takes a monadic value and function that takes
    a normal value, applies the function to the normal value and
    returns the result as a monadic value.
    
    Which means (>>=) would take a 'Maybe a' value and a function
    'a -> Maybe b' and return a 'Maybe b' value
    
-}
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

ex8 = applyMaybe (Just 3) (\x -> Just (x+1))        -- Just 4
ex9 = applyMaybe (Just "smile") 
                 (\x -> Just (x ++ " :)"))          -- Just "smile :)"
ex10 = applyMaybe Nothing (\x -> Just (x+1))        -- Nothing                 
ex11 = applyMaybe (Just 3)
                  (\x -> if x > 2 then Just x else Nothing)  -- Just 3
ex12 = applyMaybe (Just 1)
                  (\x -> if x > 2 then Just x else Nothing)  -- Nothing

{-
    The Monad instance of Maybe is declared as:
    
        instance Monad Maybe where  
            return x      = Just x  
            Nothing >>= f = Nothing  
            Just x  >>= f = f x  
            fail _        = Nothing      

-}           
ex13 = return "WHAT" :: Maybe String        -- Just "WHAT"
ex14 = Just 9  >>= \x -> return (x*10)      -- Just 90
ex15 = Nothing >>= \x -> return (x*10)      -- Nothing   

{-
    [Note: 'return' just wraps a value in Just; IT DOES NOT STOP
           EXECUTION!]
-}
--
-- example using (>>=) to handle a series of actions
-- which simulate birds landing on the left or right side of a 
-- tightrope walker's balancing pole
--
type Birds = Int
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

b1 = landLeft    2  (0,0)                           -- (2,0)
b2 = landRight   1  (1, 2)                          -- (1,3)
b3 = landRight (-1) (1,2)                           -- (1,1)
b4 = landLeft 2 (landRight 1 (landLeft 1 (0,0)))    -- (3,1)

-- parameter op function
x -: f = f x

b5 = 100   -: (*3)               -- 300
b6 = True  -: not                -- False
b7 = (0,0) -: landLeft 2         -- (2,0)

-- such a function lets us write a series of bird landings
-- [Note: we only need to supply need to provide the first pole
--        as subsequent poles are generated as the result of the
--        (-:) operation]
b8 = (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2       -- (3,1)

-- now, we really want the landing functions to include a notion
-- of failure i.e.  we want to capture the fact that too many birds
-- on one side of the pole will cause the tightrope walker to fall
-- to do this we beef them up and change the Pole to a Maybe Pole
--
landLeft' :: Birds -> Pole -> Maybe Pole  
landLeft' n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight' :: Birds -> Pole -> Maybe Pole  
landRight' n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

b9  = landLeft'  2 (0,0)         -- Just (2,0)
b10 = landLeft' 10 (0,3)         -- Nothing

-- we can now use the bind operator to sequence actions
-- [Note: again, only the first pole is required as the result
--        of each function application (>>=) is passed along the
--        chain]
b11 = landRight' 1 (0,0) >>= landLeft' 2                  -- Just (2,1)
b12 = Nothing >>= landLeft' 2                            -- Nothing
b13 = return (0,0) >>= landRight' 2  >>= landLeft' 2 >>= landRight' 2 


-- one failure in the sequence causes the entire sequence to fail                   
b14 = return (0,0) >>= landLeft' 1 >>= landRight' 4       -- Nothing
              
             
-- we can also create a function that just makes the tightrope walker
-- fail regardless of the number of birds on the pole
banana :: Pole -> Maybe Pole
banana _ = Nothing

b15 = return (0,0) >>= landLeft' 1 >>= banana >>= landRight' 1 -- Nothing


-- another option is to simply use the sequential application operator
-- without a bind; that let's us ignore the result of a preceding action
b16  = return (0,0)             -- initial Pole
   >>= landLeft' 1              -- bird landing
   >> Nothing                   -- ignore previous bird landing, force fail
   >>= landRight' 1             -- final result is a fail
 
-- we could also use 'do' notation for our sequencing as it is
-- just syntactic sugar for (>>=) and (>>)
-- [Note: the value of (<-) depends on the definition of (>>=)]
routine :: Maybe Pole  
routine = do  
    start  <- return (0,0)  
    first  <- landLeft'  2 start  
    second <- landRight' 2 first  
    landLeft' 1 second
    
ex17 = routine      -- Just (3,2)

routine2 :: Maybe Pole  
routine2 = do  
    start <- return (0,0)           -- start = Just (0,0)
    first <- landLeft' 2 start      -- first = Just (2,0)
    Nothing                         -- equiv to using (>>)
                                    -- or writing: _ <- Nothing
                                    -- value of 'first' is not passed on
    second <- landRight' 2 first    
    landLeft' 1 second              
    
ex18 = routine2                     -- Nothing

{-
    Why is the result of the above Nothing?? Looks as if it should
    be (3,2)
    
    [Note: if you think imperatively this can really throw you!
    
           first take is that the Nothing value is just ignored
           output while 'first' and 'second' are valid variable
           values that can be used by landRight' and landLeft'
           
           Completely wrong intuition!]
           
    Because the 'do' block is syntactic sugar for the following:
-}
ex18a = return (0,0)                    -- return is applied and
    >>= \start -> landLeft' 2 start     -- result bound to 'start'
    >>= \first -> Nothing               -- result of prev op bound to first
    >>  landRight' 2 first              -- result 'first' fed to Nothing
    >>= \second -> landLeft' 1 second   -- so result 'second' is Nothing

-- same without 'variable' capture    
ex18b = return (0,0) >>= landLeft' 2  >> Nothing 
                     >>= landRight' 2 >>= landLeft' 1  -- Nothing
           
{-
    do Notation and pattern matching
-}
justH :: Maybe Char
justH = do (x:xs) <- Just "hello"
           return x
           
ex19 = justH        -- Just 'h'

noH :: Maybe Char
noH = do (x:xs) <- Just ""          -- empty String, no 'x' value
         return x
            
ex20 = noH          -- Nothing
            
{-
    The List Monad
    --------------
    Lists represent non-deterministic values when used as applicatives
    [i.e. they represent no specific value which is the same as saying
          they represent an infinite number of values; all the values
          we from which we could possibly pick, etc.]
    So, a value like '5' is 'deterministic', or a 'determined' value
    while [3,8,9] contains possible results or 'undetermined results'
    [it's not determined until we apply a function??]
          
    Lists used as applicative functors nicely represents non-deterministic
    behaviour
-}        
ex21 = (*) <$> [1,2,3] <*> [10,100,1000]
    -- [10,100,1000,20,200,2000,30,300,3000]

    
{-
    The Monad instance for a list is declared as:
    
        instance Monad [] where  
            return x = [x]  
            xs >>= f = concat (map f xs)  
            fail _ = []    

-}
-- f is mapped to every value of the list and the results of 
-- each mapping are concatenated back into a list
ex22 = [3,4,5] >>= \x -> [x,-x]     -- [3,-3,4,-4,5,-5]

-- an empty list signals failure, just like Nothing did
ex23 = []      >>= \x -> ["bad","mad","rad"]     -- []
ex24 = [1,2,3] >>= \x -> []                      -- []

-- we can chain list actions
-- here, the digits [1,2] are bound to var 'n'
--       the chars ['a','b'] are bound to var 'ch'
-- apply each element in the first list to every element in 
-- the second list and then concatenate the results of the 
-- individual visits
-- [Note: the result is the same as the list comprehension
--        [(x,y) | x <- [1,2], y <- ['a','b']] ]
ex25 = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
        -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- previous example written in do notation
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n  <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)

ex26 = listOfTuples         -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

{-
    MonadPlus Class
    ---------------
    A class for Monads that act as Monoids
    
        class Monad m => MonadPlus m where  
            mzero :: m a                        -- equiv to mempty
            mplus :: m a -> m a -> m a          -- equiv to mappend
            
    comes with a 'guard' method which can be used to 'filter'
    monad sequences
    
        guard :: (MonadPlus m) => Bool -> m ()  
        guard True  = return ()  
        guard False = mzero 
-}
ex27 = guard (5 > 2) :: Maybe ()            -- Just ()
ex28 = guard (1 > 2) :: Maybe ()            -- Nothing
        
{-            
    The List instance of MonadPlus
    
        instance MonadPlus [] where  
            mzero = []  
            mplus = (++)      

-}
ex29 = guard (5 > 2) :: [()]                -- [()]
ex30 = guard (1 > 2) :: [()]                -- []

-- we can use guard to filter out non-deterministic computations
-- here, we only return 'x' if it contains a 7
-- equiv to [ x | x <- [1..50], elem '7' (show x)]
ex31 = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
    -- [7,17,27,37,47]
    
-- How does that work??
-- in the following, guard returns [()], this is discared by (>>)
-- and "cool" is returned BUT if 'guard' fails, then so too will return
ex32 = guard (5 > 2) >> return "cool" :: [String]   -- ["cool"]
ex33 = guard (1 > 2) >> return "cool" :: [String]   -- []

-- rewriting ex31 using 'do' notation
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x
    
ex34 = sevensOnly               -- [7,17,27,37,47]

-- An example based on moving knight around an empty chess board
-- The knight's position is represented by (column, row)
-- want to see if the knight can get to a specified positio within
-- three moves
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do 
    -- all possible moves from current position
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    -- keep them on the board
    guard  (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  
   
-- another way to write the above using a helper and filter function
moveKnight' :: KnightPos -> [KnightPos]  
moveKnight' (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]   
    
km1 = moveKnight (6,2) == moveKnight' (6,2)     -- True
km2 = moveKnight (8,1) == moveKnight' (8,1)     -- True

-- a function that returns all possible positions for 3 moves
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second  
    
km3 = in3 (6,2)

-- the same function without the do notation
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

km4 = km3 == in3' (6,2)     -- True

-- a function to say if we can reach a specific position in three moves
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  
    
km5 = canReachIn3 (6,2) (6,1)       -- True
km6 = canReachIn3 (6,2) (7,3)       -- False




    
    