{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

{-
    Week 07 - Folds and Monoids
              Homework Exercise 3
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/hw/
        07-folds-monoids.pdf
-} 
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)
  
tiles :: [(Char, Int)]  
tiles = [ ('A',1),('B',3),('C',3),('D',2),('E',1),('F',4),
          ('G',2),('H',4),('I',1),('J',8),('K',5),('L',1),
          ('M',3),('N',1),('O',1),('P',3),('Q',10),('R',1),
          ('S',1),('T',1),('U',1),('V',4),('W',4),('X',8),
          ('Y',4),('Z',10) ]           

getScore :: Score -> Int
getScore (Score i) = i
          
score :: Char -> Score
score chr = Score n
    where 
     -- n  = foldr (+) 0 $ map (\ (c,v) -> if c == ch then v else 0) tiles
        n  = sum . map snd $ filter ((== ch) . fst) tiles
        ch = toUpper chr

scoreString :: String -> Score
scoreString []     = 0
scoreString (x:xs) = score x `mappend` scoreString xs




