-- Operations involving Strings
module Strings where

import Data.Char (isUpper)
import Data.List (isPrefixOf)
import Data.Maybe
import Text.Read  ( readMaybe )

{- References:
    [CIS194] CIS 194: Introduction to Haskell (Fall 2014)
    [DM]     Discrete Math using a Computer
    [HR]     The Haskell Road to Logic, Math, and Programming
    [INFP]   Informations Functional Programming Course (inf1fp)
    [TDSL]   Two Dozen Short Lessons in Haskell
    
-}

-- return True if all words are capitalized
allCaps :: [String] -> Bool
allCaps = all isUpper . map (fromMaybe 'a') . map safeHead

-- drop trailing whitespace [CIS194]
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = takeWhile (/= ' ')

-- find the start position of a string within another string [DM]
find :: String -> String -> Maybe Int
find str [] = Nothing
find [] _   = Nothing
find str substr = if isPrefixOf substr str then Just 0 else f 1 str
    where
        f idx (x:xs) | isPrefixOf substr xs = Just idx
                     | otherwise            = f (idx + 1) xs
        f _ _ = Nothing
      

-- get the first letter from each string in a list of strings [CIS194]
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe safeHead

-- return characters that match the given character [TDSL]
matchChar :: Char -> String -> String
matchChar chr str = [c | c <- str, c == chr]

-- count uppercase letters in a list of strings [CIS194]
numUppercase :: [String] -> Int
numUppercase = length . filter isUpper . concat

-- remove any character [TDSL]
remove char str = [c | c <- str, c /= char]

-- remove punctuation characters [TDSL]
removePunctuation str = foldr1 (.) [remove c | c <- ",. \'?\"!;:()"] str

-- safe head function for lists [CIS194]
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- safe tail function for lists [CIS194]
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- return the index position of the given character [INFP]
search :: Eq a => [a] -> a -> [Int]
search xs y = [ i | (i,x) <- zip [0..] xs, x==y ]

--    Converts a string in the form of a1a2a3... to a string
--    in the form a1a2a2a3a3a3...  [HR]
spread :: [a] -> [a]
spread xs = [ x | (n,y) <- zip [1..] xs , x <- take n (repeat y)]    

-- remove blank characters from a string [TDSL]
stripBlank :: String -> String
stripBlank str = [c | c <- str, c /= ' ']

-- remove the given character from a string [TDSL]
stripChar :: Char -> String -> String
stripChar chr str = [c | c <- str, c /= chr]

-- find all digits in a string and add them together [CIS914]
sumStringDigits :: String -> Int
sumStringDigits = sum . mapMaybe read_digit
  where read_digit :: Char -> Maybe Int
        read_digit = readMaybe . listify

        listify :: a -> [a]
        listify x = [x]          -- can also be written as (:[]) x
 


