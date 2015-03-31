{-
    Week 11 Applicative Functors - Part 2
        Notes and exercises
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        11-applicative2.html        
-}
{-

    Definitions of Functor and Applicative
    --------------------------------------
    
        class Functor f where
          fmap :: (a -> b) -> f a -> f b

        class Functor f => Applicative f where
          pure  :: a -> f a
          (<*>) :: f (a -> b) -> f a -> f b    

    Every Applicative is also a Functor and
        
        fmap g x = pure g <*> x
        
    must hold.
    
    Applicative definition for lists
    --------------------------------
    
    instance Applicative [] where
        pure a        = [a]          -- a "deterministic" value
        [] <*> _      = []
        (f:fs) <*> as = (map f as) ++ (fs <*> as)    
          
          
    for this instance, each functor is applied to every element
    in the given list
-}
import Control.Applicative
import AParser

type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

names = ["Joe", "Sara", "Mae"]
phones = ["555-5555", "123-456-7890", "555-4321"]

employees1 = Employee <$> names <*> phones
-- doesn't make much sense as gives every possible combination of the
-- two

-- non-deterministic arithmetic
(.+) = liftA2 (+)    -- addition lifted to some Applicative context
(.*) = liftA2 (*)    -- same for multiplication

-- nondeterministic arithmetic
-- (either 4 or 5) times 2, plus either 6 or 1
n = ([4,5] .* pure 2) .+ [6,1]      -- [14,9,16,11]

-- and some possibly-failing arithmetic too, just for fun
-- these won't compile, expects [a] not Maybe a
--m1 = (Just 3 .+ Just 5)  .* Just 8
--m2 = (Just 3 .+ Nothing) .* Just 8

{-

    Implementing Applicative on lists to work in a postion to 
    position (deterministic) manner
    
        newtype ZipList a = ZipList { getZipList :: [a] }
          deriving (Eq, Show, Functor)

        instance Applicative ZipList where
          pure = ZipList . repeat
          ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)    

-}

employees2 = getZipList $ Employee <$> ZipList names <*> ZipList phones

{-

    An example for ((->) e), the 'reader' or 'environment' applicative
    as it allows 'reading' from an environment 'e'.

        instance Functor ((->) e) where
          fmap = (.)

        instance Applicative ((->) e) where
          pure = const
          f <*> x = \e -> (f e) (x e)
  
-}
data BigRecord = BR { getName         :: Name
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getPhone        :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    }

r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

ex01 = getEmp r

{-
    Applicative (and Monad) is a "model of computation"; it abstracts
    out the details. Once we have an Applicative instance for a
    type we can forget the underlying details and just write to 
    what we need using the Applicative interface. This allows us
    to write generic tools that will work with any type.
    
    The following is an example for pairs.

-}
pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)

-- examples of using pair with Maybe
exp1  = pair (Just 1) (Just 2)          -- Just (1,2)
exp1a = pair (Just 3) Nothing           -- Nothing

-- with []
exp2 = pair [1..2] ['A'..'C']
    -- (1,'A'),(1,'B'),(1,'C'),(2,'A'),(2,'B'),(2,'C')]

-- using ZipList directly, without 'pair' function    
exp2a = getZipList $ (,) <$> ZipList [0..2] <*> ZipList ['A'..'C']
    -- [(0,'A'),(1,'B'),(2,'C')]

-- using ZipList with 'pair' function    
exp2b = getZipList $ pair (ZipList [0..2]) (ZipList ['A'..'C'])
    -- [(0,'A'),(1,'B'),(2,'C')]

-- using IO
exp3 = pair getLine getLine

{-
    *Main> exp3
    2
    3
    ("2","3")
    *Main> 

-}
exp4 = pair (putChar 'a') (putChar 'c')
{-

    *Main> exp4
    ac((),())
    *Main> 

-}    

-- using 'pair' with Parser
exp5 = pair (runParser posInt "123ab") (runParser posInt "4ab3c")
    -- Just ((123,"ab"),(4,"ab3c"))

exp5a =  pair (runParser posInt "123ab") (runParser posInt "ab3c")  
    -- Nothing

-- using (*>), which drops the first result of the first argument

ex6a = (*>) (Just 1) (Just 2)                                  -- Just 2
ex6b = (*>) [1..2] ['A'..'C']                                  -- "ABCABC"
ex6c = getZipList $ (*>) (ZipList [0..2]) (ZipList ['A'..'C']) -- "ABC"

-- the ((),()) in the result is dropped (results in reverse order??)
ex6d = (*>) (putChar 'a') (putChar 'c')                        -- ac

ex6e = (*>)  (runParser posInt "123ab") 
             (runParser posInt "4ab3c")                -- Just (4,"ab3c")

-- same again using mapA
mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = foldr ((liftA2 (:)) . f) (pure [])

ex7a = mapA Just [1,2]               -- Just [1,2]
ex7d = mapA putChar ['a','b','c']    -- abc[(),(),()]
ex7e = mapA (runParser posInt) 
            ["123ab","4ab3c"]        -- Just [(123,"ab"),(4,"ab3c")]



        
