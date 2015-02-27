{-
    there's overlap here with L03B-TestGenerators
-}
-- using QuickCheck to test custom types
import Test.QuickCheck --hiding (OrderedList, Ordered)

{-
    QuickCheck can perform random testing for any Arbitrary type
    Any type 'a' in Arbitrary has a random value generator, 'Gen a'
    which is a monad so things of type 'Gen a' are another kind
    of instruction; ones that will build random values of type 'a'
    
    You can get samples of what QuickCheck will generate
-}
-- example of getting samples of random type values
sampleInts    = sample (arbitrary :: Gen Integer)
sampleBools   = sample (arbitrary :: Gen Bool)
sampleDoubles = sample (arbitrary :: Gen Double)

sampleListInts = sample (arbitrary :: Gen [Integer])
sampleListChars = sample (arbitrary :: Gen [Char])

-- we can write generators using the existing generators: return and do
custTrue = sample (return True)

-- the following using doTwice to create tuples of random integers
doTwice :: Monad a => a b -> a (b,b)    
doTwice io =
    do a <- io
       b <- io
       return (a,b)

custDo2 = sample( doTwice (arbitrary :: Gen Integer) )

-- writing a generator for even numbers
evenInteger :: Gen Integer
evenInteger = do n <- arbitrary
                 return (2*n)
                 
custEvenNum = sample evenInteger

-- the QuickCheck library has many functions for constructing generators
sampleChoose = sample (choose (1,10) :: Gen Integer)
sampleOneOf  = sample (oneof [return 1, return 10])
               
-- generator for a custom data type
data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq)
    
rSuit :: Gen Suit
rSuit = oneof [return Spades, return Hearts, 
               return Diamonds, return Clubs]

sampleSuits = sample rSuit

-- above can also be defined as
rSuit' :: Gen Suit
rSuit' = elements [Spades, Hearts, Diamonds, Clubs]

sampleSuits' = sample rSuit'

-- generating a Rank
data Rank = Numeric Integer | Jack | Queen | King | Ace
    deriving (Show, Eq)
    
rRank :: Gen Rank    
rRank = oneof [ return Jack,
                return Queen,
                return King,
                return Ace,
                do r <- choose (2,10)
                   return (Numeric r) ]
                   
sampleRanks = sample rRank

-- generating a Card
data Card = Card Rank Suit
    deriving (Show, Eq)

rCard :: Gen Card    
rCard = do r <- rRank
           s <- rSuit
           return (Card r s)

sampleCards = sample rCard
           
-- generating a Hand
data Hand = Empty | Add Card Hand
    deriving (Show, Eq)

rHand :: Gen Hand
rHand = oneof [ return Empty,
                do c <- rCard
                   h <- rHand
                   return (Add c h)]

sampleHands = sample rHand

{-
    Getting QuickCheck to use custom types
    --------------------------------------
    QuickCheck can generate any type of class Arbitrary
    so we need to make our types instances of this class
-}                   

instance Arbitrary Suit where
    arbitrary = rSuit
    
instance Arbitrary Rank where
    arbitrary = rRank
    
instance Arbitrary Card where
    arbitrary = rCard

instance Arbitrary Hand where
    arbitrary = rHand    
    
{-
    We can create 'datatype invariants' to ensure the generated
    values are valid for our type
-}    
validRank :: Rank -> Bool
validRank (Numeric r) = 2 <= r && r <= 10
validRank _           = True

-- testing our invariant
prop_Rank r = validRank r

{-
    If our test succeeds, we don't see any of the actual values
    to see the generated values, use 'collect'
   
-}
prop_Rank' r = collect r (validRank r)

{-
    Output:
    
    *L03C> quickCheck prop_Rank'
    +++ OK, passed 100 tests:
    25% Ace
    20% King
    19% Queen
    17% Jack
     5% Numeric 4
     3% Numeric 9
     3% Numeric 8
     2% Numeric 7
     2% Numeric 3
     1% Numeric 6
     1% Numeric 5
     1% Numeric 2
     1% Numeric 10
    
-}
{-
    From the above we can see that face cards occur far more
    frequently than other cards. We can fix this by re-writing
    rRank using 'frequency' to change the weighting
    
    rRank :: Gen Rank    
    rRank = frequency [ (1,return Jack),
                        (1,return Queen),
                        (1,return King),
                        (1,return Ace),
                        (9, do r <- choose (2,10)
                               return (Numeric r)) ]
    
-}
{-
    Using 'collect' to see generate hand values creates too 
    much data; get a summary instead

-}
numCards :: Hand -> Integer
numCards Empty = 0
numCards (Add _ h) = 1 + numCards h

prop_Hand h = collect (numCards h) True

{-
    Output:
    
        *L03C> quickCheck prop_Hand
        +++ OK, passed 100 tests:
        49% 0
        23% 1
        11% 2
         7% 3
         3% 5
         3% 4
         2% 6
         1% 8
         1% 7
    
-}
{-
    From the above, can see that over 80% have 1 card or less
    We can, again, fix this by using 'frequency' to generate
    the hands
    
        rHand = frequency [ (1, return Empty),
                            (4, do c <- rCard
                                   h <- rHand
                                   return (Add c h))]
                                   
    Returning Empty 20% of the time gives average hands of 5 cards.
    
        prop_Hand h = collect (numCards h) True
        
    is not testing any particular properties of the Hand, we
    could supply a predicate (instead of True) to test properties
    that every hand should have
    
-}
-- TESTING ALGORITHMS

insert a (x : xs)
    | a <= x    = [a, x] ++ xs
    | otherwise = [x] ++ (insert a xs)
insert a [ ] = [a]

ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)


-- a list insert should always the new value at the correct spot
-- in an ordered list; doesn't work all that well as the chances
-- of a random list being ordered is very slim
prop_insert :: Integer ->  [Integer] -> Property
prop_insert x xs = 
    collect (length xs) $ ordered xs ==> ordered (insert x xs)

{-
    Output:
    
        *L03C> quickCheck prop_insert
        *** Gave up! Passed only 75 tests:
        40% 0
        30% 1
        20% 2
         6% 3
         1% 5
         1% 4
    
-}
-- NOTE: orderedList, OrderedList and its ctor Ordered are all
--       defined in Test.QuickCheck
-- a function to generate ordered lists
orderedList' :: Gen [Integer]
orderedList' = do n <- arbitrary
                  listFrom n
               where listFrom n = frequency 
                                   [(1, return []),
                                    (5, do i <- arbitrary
                                           ns <- listFrom (n + abs i)
                                           return (n:ns))]
                                           
data OrderedList' = Ordered' [Integer]
    deriving (Show)

instance Arbitrary OrderedList' where
    arbitrary = do xs <- orderedList
                   return (Ordered' xs)
                   
prop_insert' :: Integer -> OrderedList Integer -> Property
prop_insert' x (Ordered xs) = 
    collect (length xs) $ ordered (insert x xs)

                   
                                      