-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Solve problems using both list comprehension and recursion
-- and write QuickCheck tests to ensure each definition pair
-- produces the same output when given the same input
--
-- NOTES: you can only use the stated library functions

import Data.Char
import Data.List
import Test.QuickCheck

{- 1. halveEvens

    (a) Write a function 
            halveEvens :: [Int] -> [Int] 
        that returns half of each even number in the list. 
        For example,
            halveEvens [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
        
        Your definition should use a list comprehension, not recursion. 
        You may use the functions div, mod :: Int -> Int -> Int.
    
    (b) Write a recursive version; you may use div and mod again
    
    (c) Write an appropriate QuickCheck test
-}

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs, (x `mod` 2) == 0 ]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec []     = []
halveEvensRec (x:xs) | (x `mod` 2) == 0  = x `div` 2 : halveEvensRec xs
                     | otherwise         = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs


{- 2. inRange

    (a) Write a function 
            inRange :: Int -> Int -> [Int] -> [Int] 
       to return all numbers in the input list within the range given 
       by the first two arguments (inclusive). 
       For example,
            inRange 5 10 [1..15] == [5,6,7,8,9,10]
       Your definition should use a list comprehension, not recursion.
       
    (b) Write an equivalent function inRangeRec, using recursion.
    
    (c) To confirm the two functions are equivalent, write a test 
        function prop_inRange and run the appropriate QuickCheck test.
-}

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, x >= lo && x <= hi ]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ []       = []
inRangeRec lo hi (x:xs) 
    | x >= lo && x <= hi = x : inRangeRec lo hi xs
    | otherwise          = inRangeRec lo hi xs
                        
-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRangeRec lo hi xs) == (inRange lo hi xs)


{- 3. countPositives: count up all the positive numbers in a list

    (a) Write a function countPositives to count the positive numbers in 
        a list (the ones strictly greater than 0). 
        
        For example,
            countPositives [0,1,-3,-2,8,-1,6] == 3
            
        Your definition should use a list comprehension. You may not use 
        recursion, but you will need a specific library function. 
        
    (b) Write an equivalent function countPositivesRec, using recursion 
        and WITHOUT using any library functions.
        
    (c) To confirm the two functions are equivalent, write a test 
        function prop_countPositives and run the appropriate QuickCheck 
        test.
        
    (d) Why do you think it's not possible to write countPositives using 
        only list comprehension, without library functions?
            List comprehensions must return a list; a sum is list
            reduction; returns a single number.
-}

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = sum [ 1 | x <- xs, x > 0 ]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec []     = 0
countPositivesRec (x:xs) | x > 0     = 1 + countPositivesRec xs
                         | otherwise = countPositivesRec xs   

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs


{- 4. pennypincher

    (a) Professor Pennypincher will not buy anything if he has to pay 
        more than $199.00. But, as a member of the Generous Teachers 
        Society, he gets a 10% discount on anything he buys. 
       
        Write a function pennypincher that takes a list of prices and 
        returns the total amount that Professor Pennypincher would have 
        to pay, if he bought everything that was cheap enough for him.
        
        Prices should be represented in pennies, not dollars, by integers. 
        To deduct 10% off them, you will need to convert them into floats 
        first, using the function fromIntegral.
        
        To convert back to ints, you can use the function round, which 
        rounds to the nearest integer. You can write a helper function
        discount :: Int -> Int to do this. For example,
        
            pennypincher [4500, 19900, 22000, 39900] == 41760
        
        Your solution should use a list comprehension, and you may use a 
        library function to do the additions for you.
        
    (b) Write an equivalent function pennypincherRec, using recursion 
        and WITHOUT using library functions. You may use your function 
        discount.
        
    (c) To confirm the two functions are equivalent, write a test 
        function prop_pennypincher and run the appropriate QuickCheck test.
    
-}

-- Helper function
discount :: Int -> Int
discount n = round ((fromIntegral n) * 0.9)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [ discount x | x <- xs, 
                                     discount x <= 19900]
                  
-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec []  = 0
pennypincherRec (x:xs) 
    | y <= 19900    = y + pennypincherRec xs
    | otherwise     = pennypincherRec xs
    where
        y :: Int
        y = discount x

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs


{- 5. multDigits
    
    (a) Write a function 
            multDigits :: String -> Int 
        that returns the product of all the digits in the input string. 
        If there are no digits, your function should return 1. 
        For example,
            multDigits "The time is 4:25" == 40
            multDigits "No digits here!" == 1

        Your definition should use a list comprehension. You'll need a 
        library function to determine if a character is a digit, one to
        convert a digit to an integer, and one to do the multiplication.
        
    (b) Write an equivalent function multDigitsRec, using recursion. 
        You may use library functions that act on single characters or 
        integers, but you may not use library functions that act on a list.
        
    (c) To confirm the two functions are equivalent, write a test 
        function prop_multDigits and run the appropriate QuickCheck test.
-}

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x <- xs, isDigit x ]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x  = digitToInt x * multDigitsRec xs
                     | otherwise  = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


{- 6. capitalize

    (a) Write a function 
            capitalize :: String -> String which, given a word, 
        capitalizes it. That means that the first character should be made
        uppercase and any other letters should be made lowercase. 
        For example,
            capitalize "edINBurgH" == "Edinburgh"
            
        Your definition should use a list comprehension and library 
        functions toUpper and toLower that change the case of a character.
        
    (b) Write a recursive function capitalizeRec. You may need to write a 
        helper function; of the helper function and the main function only
        one needs to be recursive.
    
    (c) To confirm the two functions are equivalent, write a test 
        function prop_capitalise and run the appropriate QuickCheck test.
-}

-- List-comprehension version
capitalize :: String -> String
capitalize (x:xs) = toUpper x : [ toLower x | x <- xs ]
capitalize _      = []

-- Recursive version
capitalizeRec :: String -> String
capitalizeRec [] = ""
capitalizeRec (x:xs) = toUpper x : lcase xs

lcase :: String -> String
lcase []     = ""
lcase (x:xs) = toLower x : lcase xs

-- Mutual test
prop_capitalize :: String -> Bool
prop_capitalize xs = capitalize xs == capitalizeRec xs


{- 7. title
    
    (a) Using the function capitalize from the previous problem, write a
        function
            title :: [String] -> [String]
            
        which, given a list of words, capitalizes them as a title should 
        be capitalized. The proper capitalization of a title (for our 
        purposes) is as follows: The first word should be capitalized. 
        Any other word should be capitalized if it is at least four 
        letters long. For example,
               title ["tHe", "sOunD", "ANd", "thE", "FuRY"]
                  == ["The", "Sound", "and", "the", "Fury"]
                  
        Your function should use a list comprehension, and not recursion. 
        Besides the capitalize function, you will probably need some 
        other auxiliary functions. You may use library functions that 
        change the case of a character and the function length.
        
    (b) Write a recursive function titleRec. You may use capitaliseRec 
        and any of its auxiliary functions.
        
    (c) To confirm the two functions are equivalent, write a test function 
        prop_title and run the appropriate QuickCheck test.
-}

-- List-comprehension version
-- Note: modified after seeing provided solution
title :: [String] -> [String]
title (x:xs) = capitalize x : [ procWord x | x <- xs ]
        where
            procWord :: String -> String
            procWord w | length w >= 4  = capitalize w
                       | otherwise      = lcase w
title _ = []

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = capitalizeRec x : capRem xs
    where 
        capRem :: [String] -> [String]
        capRem [] = []
        capRem (x:xs) | length x < 4 = lcase x : capRem xs
                      | otherwise    = capitalizeRec x : capRem xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs


-- Optional Material

{- 8. crosswordFind

    (a) Dame Curious is a crossword enthusiast. She has a long list of 
        words that might appear in a crossword puzzle, but she has trouble
        finding the ones that fit a slot. Write a function
            crosswordFind :: Char -> Int -> Int -> [String] -> [String]
        to help her. 
        
        The expression
                crosswordFind letter inPosition len words
        
        should return all the items from words which 
            (a) are of the given length, and 
            (b) have letter in the position inPosition. 
            
        For example, if Curious is looking for seven-letter words that 
        have 'k' in position 1, she can evaluate the expression:
            crosswordFind 'k' 1 7 ["funky", "fabulous", "kite", 
                                   "icky", "ukelele"]
        which returns ["ukelele"]. (Remember that we start counting with 
        0, so position 1 is the second position of a string.)
        
        Your definition should use a list comprehension. You may also use 
        a library function which returns the nth element of a list, for 
        argument n, and the function length.

    (b) Write a recursive function crosswordFindRec to the same 
        specification (you can use the same library functions).
        
    (c) Write a QuickCheck property prop_crosswordFind to test your 
        functions.
-}

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind _ _ _ [] = []
crosswordFind ltr pos len words
    = [ w | w <- words, pos > 0         -- && implied between guards
                      , pos < len               
                      ,(length w) == len 
                      , ltr == (w !! pos) ]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec ltr pos len (w:ws)   
    | pos < 0               = []
    | pos >= len            = []   
    |    w == ""
      || (length w) /= len
      || ltr /= (w !! pos)  = crosswordFindRec ltr pos len ws
    | otherwise             = w : crosswordFindRec ltr pos len ws

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind ltr pos len ws 
    = crosswordFind ltr pos len ws == crosswordFindRec ltr pos len ws


{- 9. search
    
    (a) Write a function 
            search :: String -> Char -> [Int] 
        that returns the positions of all occurrences of the second 
        argument in the first. For example,
            search "Bookshop" 'o' == [1,2,6]
            search "senselessness's" 's' == [0,3,7,8,11,12,14]
            
        Your definition should use a list comprehension, not recursion. 
        You may use the function zip :: [a] -> [b] -> [(a,b)], the 
        function length :: [a] -> Int, and the term forms [m..n] and [m..].
        
    (b) Write the recursive function searchRec. You may like to use an 
        auxiliary function in your definition, but you shouldn't use any 
        library functions.
        
    (c) Write a QuickCheck property prop_search to test your functions.
-}

-- List-comprehension version

search :: String -> Char -> [Int]
search str c = [ pos | (ltr, pos) <- xs, ltr == c ]
    where
        xs :: [(Char, Int)]
        xs = zip str [0..]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec [] _   = []
searchRec str c = srch (zip str [0..]) c
    where
        srch :: [(Char,Int)] -> Char -> [Int]
        srch ( (ltr, pos): xs) c 
            | ltr == c  = pos : srch xs c
            | otherwise = srch xs c
        srch _ _ = []

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search str c = search str c == searchRec str c


{- 10. contains

    (a) Write a function 'contains' that takes two strings and returns True 
        if the first contains the second as a substring. You can use the 
        library function 'isPrefixOf', which returns True if the second 
        string begins with the first string, and any list function on 
        page 127 of the book. For example,
   
            contains "United Kingdom" "King" == True
            contains "Appleton" "peon" == False
            contains "" "" == True 
            
        Your definition should use a list comprehension, not recursion. 
        A hint: you can use the library function drop to create a list 
        of all possible suffixes("last parts") of a string.
        
    (b) Write a recursive function to the same specification. Pay 
        attention to the last case of the above three (containsRec "" "").
        
    (c) Write a QuickCheck property prop_contains to test your 
        functions.

-}

-- List-comprehension version
-- Note: this is the provided solution
--       Remember, if necessary, you can pre-process the data before 
--       handing it to a list comprehension
contains :: String -> String -> Bool
contains xs ys =  [] /= [ True | x <- suffixes xs, isPrefixOf ys x]
    where
        suffixes :: String -> [String]
        suffixes xs = [drop i xs | i <- [0.. length xs]]

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] [] = True
containsRec [] ys = False
containsRec xs [] = True
containsRec xs ys | xs == ys         = True
                  | isPrefixOf ys xs = True
                  | otherwise        = containsRec (drop 1 xs) ys

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains xs ys = contains xs ys == containsRec xs ys


