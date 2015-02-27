module L03B where 
--------------------------
-- Topics on Monads and
-- Test Data Generation
-- Lecture 3B, 2011
-- dave@chalmers.se 
--------------------------
import Test.QuickCheck 
import Data.List (nub,insert,sort)
import Control.Monad (liftM) -- fmap

--------------------------

doTwice io = do 
     a <- io
     b <- io
     return (a,b)

dont io = return ()

-- examples print getLine
-- types 
ex1 = doTwice $ print 42

{-
    *L03B> ex1
    42
    42
    ((),())
-}

ex2 = doTwice getLine

{-
    *L03B> ex2
    10
    11
    ("10","11")
    *L03B> 
-}

ex3 =  length [doTwice $ print 42]

{-
    There is one 'instruction' in the list and it gets executed
    Output:
    
    *L03B> ex3
    1
    *L03B> 
-}

ex4 = [doTwice $ print 42]

{-
    Here, ghci doesn't know how to 'show' the instruction
    
    Output:
    
    *L03B> ex4

    <interactive>:152:1:
        No instance for (Show (IO ((), ()))) arising from a use of 
        ‘print’ In a stmt of an interactive GHCi command: print it
    *L03B> 
    
    Similar problem occurs if you try to print [abs]
    System doesn't know how to print functions
    
    Same thing for IO, we are not interested in 'printing' instructions
    we 'run' them.

-}
{-
    Type of doTwice is:
  
        doTwice :: Monad m => m t -> m (t, t)
        
    So IO is one type of Monad, there are others.

-}
{-
    QuickCheck can perform random testing on any values of type
    Arbitrary.
    
    For any type 'a' in Arbitrary there is a random value
    generator, Gen a, which is a Monad, so things of type Gen a
    are another kind of 'instruction' so we can use 'do', 'return',
    etc.
    
    The runtime system executes IO instructions
    The QuickCheck library executes Gen instructions
    
    The definition for the class Arbitrary includes a function
            arbitrary :: Gen a
            
    so we can put anything in the Arbitrary class by implementing
    an arbitrary function; QuickCheck automatically uses this function
    to generate random values
    
    QuickCheck provides functions to check generators
-}
-- Gen A
-- sample
ex5 = sample (arbitrary :: Gen Int)

{-
    *L03B> ex5
    0
    1
    -1
    6
    -3
    -24
    48
    69
    863
    1177
    -3210

-}

ex6 = sample' (arbitrary :: Gen Int)

{-
    *L03B> ex6
    [1,-2,3,-7,-22,-3,82,-46,272,929,3431]
    *L03B> 
-}

ex7 = sample' (arbitrary :: Gen (Int,[Bool]))
{-
    *L03B> ex7
    [(1,[]),
     (-1,[True]),
     (-2,[True,False,True]),(-8,[False]),
     (7,[True,True,False,True,False,False,True]),
     (-6,[False,True,True,True,False]),
     (-91,[]),
     ...]
-}

-- writing generators
-- using return, do
ex8 = sample' $ doTwice (arbitrary :: Gen Int)

{-
    Gives us a pairs of random numbers

    *L03B> ex8
    [(1,0),(2,-1),(-3,-2),(-8,4),(7,25),(24,49),(101,26),(75,-11),
    (1001,-709),(-2005,-905),(859,-2337)]
    *L03B>

-}

-- natural numbers
nats :: Gen Integer
nats = do                    -- could have used: fmap abs arbitrary
     i <- arbitrary
     return $ abs i

ex9 = sample' nats           -- [0,2,4,3,1,3,3,13,5,7,12]
     
evenInts :: Gen Integer
evenInts = do               -- could have used: fmap (2*) arbitrary
         i <- arbitrary
         return $ 2 * i
         
ex10 = sample' evenInts      -- [0,-2,-4,4,4,-16,-20,14,-2,-36,28]        

oddInts = liftM (1+) evenInts
{-
         do 
         i <- evenInts
         return $ i + 1
 -}

{-
    liftM applies a function to the value in the Monad
    fmap does the same and is defined in Prelude.hs
-}

ex11 = take 5 `fmap` getLine
{-
    Applies 'take 5' to the value IO String returned by getLine
    
    *L03B> ex11
    hellow world
    "hello"
    *L03B>     
-}

-- pattern: liftM / fmap

evenInts' :: Gen Int
evenInts' = fmap (2*) arbitrary

ex12 = sample' evenInts'      -- [-2,-4,8,-14,32,-16,-10,78,316,-3124,934]
ex13 = sample' (fmap abs (arbitrary :: Gen Int))
    -- [0,0,2,7,26,62,62,202,453,667,1460]

-- Building Generators
-- listOf, vectorOf
-- choose (,)
exChoose = sample' $ choose (0 :: Int,100 :: Int)

-- return
-- oneof
exOneof = sample' $ oneof [evenInts', fmap (\n -> (-1)* n - 1) evenInts']
exOneof1 = sample' $ oneof [evenInts', return 42]

-- elements
exElements = sample' (elements ['a'..'z'] :: Gen Char)

-- frequency
exFreq = sample' $ frequency [(1,evenInts),(9,return 42)]

---------------------------------------

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show,Eq)

instance Arbitrary Suit where
   arbitrary = rSuit

rSuit :: Gen Suit
rSuit = elements [Spades, Hearts, Diamonds, Clubs]

data Rank = Numeric Integer | Jack | Queen | King | Ace
        deriving (Show,Eq,Ord)

rRank  = elements $ map Numeric [2..10] ++ [Jack , Queen , King , Ace]
rRank' = oneof [rRoyal, rNumeric]

rRoyal = elements [Jack , Queen , King , Ace]

rNumeric = do n <- choose(2,10)
              return $ Numeric n

instance Arbitrary Rank where
  arbitrary = frequency [(4,rRoyal),(9,rNumeric)]

-- datatype invariant
prop_Rank (Numeric n) = n > 1 && n < 11
prop_Rank _           = True

-- seeing the distribution of sample data
prop_Rank'  r = collect r $ prop_Rank r
prop_Rank'' r = classify (r < Jack) "Numeric" $ prop_Rank r

data Card = Card Rank Suit   
  deriving (Show,Eq)

instance Arbitrary Card where
   arbitrary = do
             r <- arbitrary
             s <- arbitrary
             return $ Card r s

data Hand = Empty | Add Card Hand 
   deriving (Show,Eq)

instance Arbitrary Hand where
  arbitrary = do 
            cs <- arbitrary
            return $ makeHand cs  -- liftM makeHand arbitrary
    where 
        makeHand []     = Empty
        makeHand (c:cs) = Add c (makeHand $ filter (/= c) cs)

prop_Hand h = cs == nub cs where cs = cards h

-- hand to list
cards :: Hand -> [Card]
cards Empty     = []
cards (Add c h) = c : cards h

-- list of cards to Hand
toHand :: [Card] -> Hand
toHand = foldr Add Empty

{-
    Could have used
    
        instance Arbitrary Hand where
            arbitrary = (toHand . nub) `fmap` listOf arbitrary

-}

--------------------------------------------------------------


-- Testing with Data Invariants
-- properties of insert

-- How to use a different generator for lists?
-- (i) use QuickCheck function forAll [not covered here: see qc tutorial]
-- (ii) Make a new type from the old with its own generator

{- -- Insertion sort from L02A

isort []     = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
insert x (y:ys)          = y:insert x ys

-}

-- we need a better way to generate the data
prop_insert x xs  = classify (length xs < 2) "Trivial" $ 
            collect (length xs) $
            sorted xs ==> sorted $ insert x xs 
  where types = x :: Integer

sorted xs = xs == sort xs   -- inefficient! 
-- Exercise: define an O(n) version
-- Harder Exercise: define it without recursion (hint: zipWith)

data OrderedI = OrderedI [Integer]
  deriving Show
-- one would normally use "newtype" rather than 
-- "data" but the difference is only efficiency.

instance Arbitrary OrderedI where
  arbitrary = do
            xs <- arbitrary
            return $ OrderedI $ sort xs
            -- inefficient again. See slides for an O(n) version
            -- Exercise: redefine using liftM instead of do...

prop_insert2 x (OrderedI xs)  = classify (length xs < 2) "Trivial" $ 
                                collect (length xs) $
                                sorted xs ==> sorted $ insert x xs 
                   where types = x :: Integer

-- Note that QuickCheck has a predefined generator for ordered lists
-- orderedList :: (Arbitrary a, Ord a) => Gen [a]
