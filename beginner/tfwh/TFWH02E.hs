module TFWH02E where

import Data.Char (toUpper, isAlpha)

{-
    Chapter 2 - Exercises
    
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/

-}

{- Exercise A
    ‘Is a half of two plus two equal to two or three?’

        0.5 * 2 + 2   = 3   
        0.5 * (2 + 2) = 2
        
    Therefore, it's equal to both, depending on precedence
    or parentheses to remove ambiguity.

-}
{- Exercise B

    [0,1)                       -- syntactical error
    double -3                   -- not well-formed
    double (-3)                 -- Int
    double double 0             -- not well-formed
    if 1==0 then 2==1           -- syntactical error
    "++" == "+" ++ "+"          -- Bool
    [(+),(-)]                   -- [(a -> a -> a)]
    [[],[[]],[[[]]]]            -- this works when tried
              -- [[],[[]],[[[]]]] :: [[[[t]]]]
    concat ["tea","for",'2']    -- not well-formed
    concat ["tea","for","2"]    -- [String]
    
-}
{-  Exercise C
    
    Write a function
        modernize :: String -> String
    
    that takes a string like
        'The morphology of prex - an essay in meta-algorithmics'
    
    and convert it to a string like
        ‘The Morphology Of Prex - An Essay In Meta-algorithmics’

-}
modernise :: String -> String
-- could  also have used (\(x:xs) -> toUpper x : xs)
modernise = unwords . map (\xs -> toUpper (head xs) : (tail xs)) . words

exC = modernise "The morphology of prex - an essay in meta-algorithmics"
          
-- Exercise D          
-- eager evaluation alternative to lazy: head . filter p       
first :: (a -> Bool) -> [a] -> a 
first p xs | null xs   = error "Empty List"
           | p x       = x
           | otherwise = first p (tail xs)
    where
        x = head xs
 
-- eager evaluation alternative to lazy: head . filter p . map f
exD :: (a -> a) -> (a -> Bool) -> [a] -> a 
exD f p xs | null xs   = error "Empty List"
           | p x       = x
           | otherwise = exD f p (tail xs)
    where
        x = f (head xs)
        
-- Exercise E        
firstM :: (a -> Bool) -> [a] -> Maybe a
firstM p xs | null xs   = Nothing
            | p x       = Just x
            | otherwise = firstM p xs
    where
        x = head xs
        
-- Exercise F
exp' :: Integer -> Integer -> Integer
exp' x n | n == 0 = 1
         | n == 1  = x
      -- | even n  = x * x * exp' x (div n 2)
         | even n  = exp' (x*x) (div n 2)
         | odd  n  = x  * exp' x (n-1)
        
-- Exercise G
type Date = (Int, Int, Int)

showDate :: Date -> String        
showDate (d,m,y) = show d ++ suffixes !! (rem d 10) ++ " "
                          ++ months !! m
                          ++ ", "
                          ++ show y
    where
        months = ["","January","February","March","April","May",
                  "June", "July", "August", "September",
                  "October", "November", "December"]
                  
        suffixes = ["th","st","nd","rd","th","th","th","th","th","th"]
        
        
exDa = showDate (10, 12, 2013)
exDb = showDate (21, 11, 2020)       

-- Exercise H
type CIN = String

addSum :: CIN -> CIN
addSum cin = cin ++ show (checkSum cin)
    where
        checkSum (c:cs) = (getDigit c) + checkSum cs
        checkSum  _     = 0

valid :: CIN -> Bool
valid cs = cs == addSum (take 8 cs)

getDigit :: Char -> Int
getDigit c = read [c] 

exH = valid "6324513428"

-- provided solution
addSum' :: CIN -> CIN
-- 'show's give the last two digits
addSum' cin = cin ++ show (div n 10) ++ show (mod n 10)
    where n = sum (map getDigit cin)
    
exH' = addSum' "63245134"  == "6324513428" 

-- Exercise I
palindrome :: IO ()
palindrome = do putStrLn "Enter a string:"
                str <- getLine
                putStrLn (isPalindrome str)
                
-- goofed this (1) isSomething implies Bool return
--             (2) THINK IN TERMS OF HIGHER FUNCTIONS!!!        
isPalindrome cs = if txt == (reverse txt)
                  then "Yes!"
                  else "No!"
    where
        txt = stripPunctuation cs
        
        stripPunctuation (c:cs) 
            | isAlpha c = toUpper c : stripPunctuation cs
            | otherwise = stripPunctuation cs
        stripPunctuation _ = ""

-- provided solution
palindrome' :: IO ()
palindrome' = do putStrLn "Enter a string:"
                 xs <- getLine
                 if (isPalindrome' xs) 
                 then putStrLn "Yes!"
                 else putStrLn "No!"

isPalindrome' :: String -> Bool
isPalindrome' xs = ys == (reverse ys) 
    where ys = map toUpper (filter isAlpha xs)
    
    