{-
    Exercise: Scrabble
    Source:
        CIS 194: Introduction to Haskell (Fall 2014), Richard Eisenberg
        Week 2 Homework http://www.seas.upenn.edu/~cis194/lectures.html
    
    This file is based on the homework template file HW02.hs d/l from:
        http://www.seas.upenn.edu/~cis194/extras/02-dict/HW02.hs
        
    The 'Words.hs' file was d/l from
        http://www.seas.upenn.edu/~cis194/extras/02-dict/Words.hs
-}
module Scrabble where

import Words    
import Data.List
import Data.Char

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, 
-- they have different properties. Specifically, a hand is unordered 
-- whereas a word is ordered. We denote this distinction by using a type 
-- synonym to talk about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places 
-- as place holders for letters from a player's hand. Because real words 
-- do not have '?' characters, we use another type synonym to track this 
-- distinction.
type Template = String

-- An 'STemplate' is like a template, but it has markers to indicate four 
-- kinds of special board locations: double-letter noted with 'D', 
-- triple-letter noted with 'T', double-word noted with '2', and 
-- triple-word noted with '3'.
--
-- For matching, these behave just like '?' does -- they can be filled in 
-- with any letter. But, when scoring, any letter played on a 'D' gets 
-- double its value, and any letter played on a 'T' gets triple its value.
-- 
-- If any square in the template is a '2', the whole word's value is 
-- doubled; if any square in the template is a '3', the whole word's score
-- is tripled. If multiples of these special squares are in the same word, 
-- the effects multiply.
type STemplate = Template

{-
    Ex 1
    Given a String and a Hand (list of Chars), can the String be formed 
    from the characters in the Hand, taking any duplicates into account?
    
    Hint: Start by thinking what this should do if the string to be
    matched is empty. Then, what should it do if the string is non-empty?
    The 'elem' and 'delete' functions from Data.List may be
    helpful here.    
    
    Examples:
        formableBy "fun" ['x','n','i','f','u','e','l'] == True
        formableBy "haskell" ['k','l','e','h','a','l','s'] == True
        formableBy "haskell" ['k','l','e','h','a','y','s'] == False

-}
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy _ [] = False
formableBy (x:xs) ys
    | x `elem` ys = formableBy xs (delete x ys)
    | otherwise   = False

{-
    Ex 2
    Using formableBy, write a function wordsFrom that
    gives a list of all valid Scrabble words formable from a certain hand.
    
    The Words module (imported by the HW02.hs you downloaded) allows
    you to use allWords :: [String], which contains all valid
    Scrabble words.
    
    Examples:
        wordsFrom ['a','b','c','d'] 
           == ["ab","ad","ba","bad","cab","cad","dab"]
           
        wordsFrom ['h','e','l','l','o'] ==
            [ "eh","el","ell","he","hell","hello","helo"
            , "ho","hoe","hole","lo","oe","oh","ole" ]
    
    Note: the homework template included this function definition
          note that 'formableBy' MUST be passed to the filter function
          as an infix operator
 
-}    
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

{-
    Ex 3
    Write a function 'wordFitsTemplate' that checks to see if a given
    word matches a template, given a set of tiles available.
    
    Examples:
        wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" 
            == True
        wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" 
            == False
        wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" 
            == False
        wordFitsTemplate "let" ['x','x'] "let" == True
-}
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate (t:ts) h (c:cs)
    | t /= '?' && t == c     = wordFitsTemplate ts h cs 
    | t == '?' && c `elem` h = wordFitsTemplate ts (delete c h) cs 
    | otherwise              = False 
wordFitsTemplate _ _ _ = False   

{-
    Ex 4
    Using the 'wordFitsTemplate' function, write another one that 
    produces all valid Scrabble words that match a given template 
    using a hand of available tiles. This will be similar to wordsFrom.
    
    Example:
        wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] ==
            ["acre","bare","carb","care","carl","earl"]
-}
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h
    -- filter operation in wordList grabs all chars not equal to '?'
    -- appends them to the hand, passing the new hand to wordsFrom
    -- the second filter compares each word in the wordList to the
    -- template, adding them to the result list only if they fit
    = let wordList = wordsFrom (filter (/= '?') t ++ h)
      in  filter (wordFitsTemplate t h) wordList
                            
{-
    Ex 5
    
    The Words module, along with providing allWords, provides 
        scrabbleValue :: Char -> Int 
    that gives the point value of any letter. Use that function to write 
    a new function that gives the point value of any word.
    
    Examples:
        scrabbleValueWord "care" == 6
        scrabbleValueWord "quiz" == 22
-}                            
scrabbleValueWord :: String -> Int
scrabbleValueWord []     = 0
scrabbleValueWord (c:cs) = scrabbleValue c + scrabbleValueWord cs

{-
    Ex 6
    
    Use the 'scrabbleValueWord' function to write a filtering function that 
    takes a list of words and selects out only those that have the maximum
    point value. Note that there may be many words tied for the most 
    points; your function must return all of them.    
    
    A helper function with an accumulator may come in handy here,
    but there are other possible solutions.
    
    Examples:
        bestWords (wordsFittingTemplate "??r?" 
                   ['c','x','e','a','b','c','l']) == ["carb"]
        bestWords ["cat", "rat", "bat"] == ["bat","cat"]
        bestWords [] == []
    
    Note: not sure if there is some way to combine map and
          filter functions to reduce list traversals; my
          solution creates a word value list (making one pass
          through the word list) and then walks through it and
          the word list again to extract only those words with
          the highest value
-}
bestWords :: [String] -> [String]
bestWords [] = []
bestWords ws = let wv  = map scrabbleValueWord ws  -- word values
                   max = maximum wv                -- max word value
               in findBestWords max wv ws
    where           
        findBestWords :: Int -> [Int] -> [String] -> [String]
        findBestWords max (v:vs) (w:ws)
            | v >= max  = w : findBestWords max vs ws
            | otherwise = findBestWords max vs ws
        findBestWords _ _ _ = []

{-
    Ex 7
    Write a function scrabbleValueTemplate that computes the value
    of playing a given word on a given template. In this function, you
    may assume that the word actually matches the template.
    
    In the STemplate,
    
        D = double-word value
        T = triple-word value
        2 = double-letter value
        3 = triple-letter value
        
    If a template has a D and a 2 or 3 then the whole word's value
    is doubled, including the 2 or 3 letter values. Same if there
    is a T, the word value is tripled.
    i.e. a double letter becomes a 4 if a D appears in the template
         or a 6 if a T appears
         a triple letter becomes a 6 or 9 if a D or T appears in the
         same template
    
    Examples:
        scrabbleValueTemplate "?e??3" "peace" == 11
        scrabbleValueTemplate "De?2?" "peace" == 24
        scrabbleValueTemplate "??Tce" "peace" == 27
-}        
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate t [] = 0
scrabbleValueTemplate t cs 
    = let multiplier = if 'T' `elem` t 
                       then 3 else if 'D' `elem` t
                                   then 2 else 1
      in swordValue multiplier t cs
    where
        swordValue :: Int -> STemplate -> String -> Int
        swordValue m (t:ts) (c:cs)
            | isDigit t = m * digitToInt t * scrabbleValue c 
                        + swordValue m ts cs
            | otherwise = m * scrabbleValue c 
                        + swordValue m ts cs
        swordValue _ _ _ = 0

{-
    Optional Exercise
    
    The templates used above are a little silly, given the way Scrabble
    works. For example, if you could use the template ?e???, then you
    could certainly use ?e?? and leave the last square blank.
    
    Write a version of wordFitsTemplate that does not require that the
    word takes up the entire template.
    
    Example:
      wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" == True
       
-}                            
wordFitsTemplate' :: Template -> Hand -> String -> Bool
wordFitsTemplate' [] _ [] = True

-- word completed and only wild cards remain in the template
wordFitsTemplate' ts _ [] = all(== '?') ts

wordFitsTemplate' (t:ts) h (c:cs)
    | t /= '?' && t == c     = wordFitsTemplate' ts h cs 
    | t == '?' && c `elem` h = wordFitsTemplate' ts (delete c h) cs 
    | otherwise              = False 
wordFitsTemplate' _ _ _ = False   


