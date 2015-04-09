{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
    Week 12 Monads
        Homework - provided file
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/
        lectures/12-monads.html
    https://www.seas.upenn.edu/~cis194/fall14/spring13/hw/12-monads.pdf
-}
{-
    General Info
    ------------
    
    The rules of attacking in Risk are as follows.

    • There is an attacking army (containing some number of units) and
      a defending army (containing some number of units).

    • The attacking player may attack with up to three units at a time.
      However, they must always leave at least one unit behind. That
      is, if they only have three total units in their army they may only
      attack with two, and so on.

    • The defending player may defend with up to two units (or only
      one if that is all they have).

    • To determine the outcome of a single battle, the attacking and
      defending players each roll one six-sided die for every unit they
      have attacking or defending. So the attacking player rolls one, two,
      or three dice, and the defending player rolls one or two dice.

    • The attacking player sorts their dice rolls in descending order. The
      defending player does the same.
      
    • The dice are then matched up in pairs, starting with the highest
      roll of each player, then the second-highest.

    • For each pair, if the attacking player’s roll is higher, then one of
      the defending player’s units die. If there is a tie, or the defending
      player’s roll is higher, then one of the attacking player’s units die.

    For example, suppose player A has 3 units and player B has 5. A
    can attack with only 2 units, and B can defend with 2 units. So A
    rolls 2 dice, and B does the same. Suppose A rolls a 3 and a 5, and B
    rolls a 4 and a 3. After sorting and pairing up the rolls, we have
    
                                A B
                                5 4
                                3 3
    
    A wins the first matchup (5 vs. 4), so one of B’s units dies. The second
    matchup is won by B, however (since B wins ties), so one of A’s
    units dies. The end result is that now A has 2 units and B has 4. If
    A wanted to attack again they would only be able to attack with 1
    unit (whereas B would still get to defend with 2—clearly this would
    give B an advantage because the higher of B’s two dice rolls will get
    matched with A’s single roll.)      

-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Control.Applicative ((<$>),(<*>),pure)
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Homework Exercises ----------------------------------------
-- Exercise 1
--      Install the Monad.Random package
--          cabal install Monad.Random

{- Exercise 2

    Given the definitions

        type Army = Int
        data Battlefield = Battlefield { attackers :: Army,
                                         defenders :: Army }

    (which are also included in Risk.hs), write a function with the type

        battle :: Battlefield -> Rand StdGen Battlefield

    which simulates a single battle (as explained above) between two
    opposing armies. That is, it should simulate randomly rolling the
    appropriate number of dice, interpreting the results, and updating
    the two armies to reflect casualties. You may assume that each player
    will attack or defend with the maximum number of units they are
    allowed.

-}
battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) = do
    aScore <- sort <$> dice (min 3 $ a)
    dScore <- sort <$> dice (min 2 $ d)
    let pairs = zip aScore dScore
    return b

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die   -- same as: sequence (replicate n die)

--attack :: Army -> Army -> [(Rand StdGen DieValue, Rand StdGen DieValue)]
attack a d = (dice (min 3 $ a) , dice (min 2 $ a) )

main :: IO ()
main = do
    --a <- evalRandIO (dice 3)
    --print (unDV <$> a)
    
    putStrLn "done"

-- --------------------------------------------------------------
-- Getting a handle on the types
-- Ref:
-- http://stackoverflow.com/questions/28103118/building-a-rand-stdgen-int
-- --------------------------------------------------------------
die1    = DV 1                  -- an example die
die1val = unDV $ die1           -- extracting a die value

-- evalRandIO executes the die 
die2 = do a <- evalRandIO die; return (unDV $ a)

roll = do r <- evalRandIO (dice 3)
          return (unDV <$> r)
    -- as r: [DV {unDV = 1},DV {unDV = 5},DV {unDV = 5}]
    -- as unDv <$> r: [1,5,5]
    
roll1 = sort <$> dice 3  
           

    