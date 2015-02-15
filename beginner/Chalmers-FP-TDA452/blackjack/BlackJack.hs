-- Lab Exercise from Chalmers 2014 TDA452 Functional Programming Course
-- Source:
--  http://www.cse.chalmers.se/edu/year/2014/
--        course/TDA452_Functional_Programming/labs/2/
module BlackJack where
{-
    Walkthrough hand construction:
    
    hand2 = Add (Card (Numeric 2) Hearts)
                (Add (Card Jack Spades) Empty)
                
        size hand2
     -> size (Add card hand) = 1 + size hand
     -> size (Add (Card (Numeric 2) Hearts)
                  (Add (Card Jack Spades) Empty)) =
            1 + size (Add (Card Jack Spades) Empty)
     -> 1 + size (Add (Card Jack Spades) Empty) = 
            1 + size Empty
     -> 1 + 1 + size Empty = 0
     -> 1 + 1 + 0
     -> 2
-}

import System.Random
import Test.QuickCheck
import Cards
import Wrapper

-- Example Hands
hNoAces = Add (Card Jack Spades)
              (Add (Card (Numeric 2) Hearts) Empty)
hOneAce = Add (Card Jack Spades)
              (Add (Card Ace Hearts) Empty)
hTwoAces = Add (Card Jack Spades)
               (Add (Card Ace Spades)
                  (Add (Card Ace Hearts) Empty))               

-- returns an empty hand
empty :: Hand
empty = Empty

-- return the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

-- return the value of a Card
valueCard :: Card -> Integer
valueCard = valueRank . rank

-- return the number of Aces
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r s) hand) 
    | r == Ace  = 1 + numberOfAces hand
    | otherwise = numberOfAces hand
    
-- return the value of a hand
value :: Hand -> Integer
value Empty                         = 0
value fh@(Add card hand) 
    | (rank card) == Ace && twoAces = 1 + value hand
    | otherwise                     = valueCard card + value hand
    where
        twoAces = (numberOfAces fh) >= 2

-- player busted?
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- given the guest hand and bank hand, who wins?
winner :: Hand -> Hand -> Player
winner g b | gameOver g            = Bank
           | gameOver b            = Guest
           | (value g) > (value b) = Guest
           | otherwise             = Bank

-- puts the first hand on top of the second hand
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2      = h2
(<+) (Add c h1) h2 = Add c (h1 <+ h2)

-- function should be associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- the size of the combined hands should equal the sum of the
-- individual hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

-- return a full deck of cards
fullDeck :: Hand
fullDeck = foldr (<+) Empty [suits Diamonds, suits Hearts,
                             suits Clubs,    suits Spades]
    where
    suits s = foldr (<+) Empty 
               (   [ Add (Card (Numeric n) s) Empty | n <- [2..10]]
                ++ [ Add (Card r s) Empty | r <- [Jack, Queen, King, Ace]])

-- draw one card from the deck, returning the new deck and hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand        = (Empty, hand)
draw (Add c deck) hand = (deck, Add c hand)

-- play the bank's hand
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty
    where
        playBank' deck bankHand 
            | (value bankHand) <= 16 = playBank' deck' bankHand'
            | otherwise              = bankHand
            
            where (deck', bankHand') = draw deck bankHand
    
-- Source for shuffle routines:
--  https://github.com/tdeekens/tda452-code/blob/master/Lab-2/BlackJack.hs    
-- Shuffles a hand of cards using a random number generator
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g unShuffled = Add removed (shuffle g' partial)
    where
        handSize           = size unShuffled
        (idx, g')          = randomR (1, handSize) g
        (partial, removed) = removeCard unShuffled idx Empty

-- Removes a card from hand1 at given position and moves it to hand2.
-- Returns a tuple of the changed Hand1 and removed card
removeCard :: Hand -> Integer -> Hand -> (Hand, Card)
removeCard (Add card h1) idx h2
    | idx == 1  = (h1 <+ h2, card)
    | otherwise = removeCard h1 (idx-1) (Add card h2)
            
-- check that if a card is in the hand before a shuffle,
-- it remains in the hand after the shuffle
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h    
    
-- returns True if the card is in the hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
    
-- check that the hand retains all the original cards after a shuffle
-- [Note: the prop_shuffle_sameCards guarantees all cards are included]
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

-- package everything up
implementation = Interface
    { iEmpty    = empty
    , iFullDeck = fullDeck
    , iValue    = value
    , iGameOver = gameOver
    , iWinner   = winner
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffle
    }
    
main :: IO ()
main = runGame implementation    