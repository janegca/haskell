module TFWH01E where

import Data.Char (toLower)
import Data.List (sort)

{-
    Chapter 1 - Exercises
    
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/

-}
-- Exercise A
double :: Int -> Int
double x = 2 * x

a1 = map double [1,4,4,3]               -- [2,8,8,6]
a2 = map (double . double) [1,4,4,3]    -- [4,16,16,12]
a3 = map double []                      -- []

-- text says all of these hold and are equivalent to
--      a * (x + y) = a*x + a*y     sum . map double = double . sum
--      x + (y + z) = (x + y) + z   sum . map sum    = sum . concat
--      x + y       = y + x         sum . sort       = sum

a4 xs = (sum . map double) xs == (double . sum) xs  -- True
a5 xs = (sum . map sum) xs    == (sum . concat) xs  -- Error
a6 xs = (sum . sort) xs       == sum xs             -- True

-- Exercise B
--      sin theta^  and (sin theta)^2
--      sin (2 * theta) / (2 * pi)

-- Exercise C
--      [1,2,3] ++ [3,2,1]   -> [1,2,3,3,2,1]
--      "Hello" ++ " World!" -> "Hello World!"
--      [1,2,3] ++ []        -> [1,2,3]
--      "Hello" ++ "" ++ "World!" -> "HelloWorld!"
c1 = [1,2,3] ++ [3,2,1]             -- [1,2,3,3,2,1]
c2 = "Hello" ++ " World!"           -- "Hello World!"
c3 = [1,2,3] ++ []                  -- [1,2,3]
c4 = "Hello" ++ "" ++ "World!"      -- "HelloWorld!"

-- Exercise D
--      words . map toLower == map (map toLower) . words

{- 
    Exercise E
    
        Identity elements for:
            addition        -> 0
            multiplication  -> 1
            concatenation   -> []
            functional comp -> id  (the function)
-}

{- 
    Exercise F
    
        anagrams :: Int -> [Word] -> String

    Given: word length sought, list of words in alpha order
    Wanted: a string of words, each with the same letters

        getByLength :: Int -> [Word] -> [Word]
            - get all words of the required n-length
            
        sortLetters :: [Word] -> [Word]
            - sort the letters in each individual word
            - need a sortString :: String -> String helper function
            
        sortWords   :: [Word] -> [Word]
            - sort the word list
            
    Provided Solution:
    
    1. Extract the words by length
            getWords :: Int -> [Word] -> [Word]
            
    2. Turn each word into a pair with the first element of the pair
       being all the letters in the word in sorted order
            addLabel :: [Word] -> (Label, Word)
            where 
                type Label = [Char]
            
    3. Sort the labelled words
            sortLabels :: [(Label,Word)] -> [(Label,Word)]
            
    4. Replace each group of adjacent labelled words with a single
       entry having the original label and a list of all related
       word
            groupByLabel :: [(Label,Word)] -> [(Label, [Word])]
            
    5. Replace each entry by a string
            showEntry :: [(Label, [Word])] -> String
            
        and concat the results.
            
    That gives
        anagrams :: Int -> [Word] -> [Word]
        anagrams n = concat 
                   . map showEntry
                   . groupByLabel 
                   . sortLabels 
                   . map addLabel
                   . getWords n
       
-}
-- Exercise G
song :: Int -> String
song n = if n == 0 then ""
         else song (n-1) ++ "\n" ++ verse n
      
verse :: Int -> String      
verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

line1, line2, line3, line4 :: Int -> String
line1 n | n == 1    = "One man went to mow\n"
        | otherwise = (digits !! n) ++ " men went to mow\n"
        
line2 _ = "Went to mow a meadow\n"  

line3 n | n == 1 = "One man and his dog\n"
        | otherwise = getPrefix n ++ "one man and his dog\n"
    where
        getPrefix m | m == 1 = ""   
                    | m == n = (digits !! m) ++ " men, " 
                             ++ getPrefix (m-1)
                    | otherwise = map toLower (digits !! m) ++ " men, "
                                ++ getPrefix (m-1)                    

line4 n = line2 n      

digits :: [String]
digits = ["", "One","Two","Three","Four","Five","Six","Seven",
          "Eight","Nine"]         
