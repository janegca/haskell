-- Informatics 1 - Functional Programming 
-- Tutorial 4 - Review of Characters and Strings
--              Listh comprehension, recursio and higher order funcstions
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/revision/tutorial5.pdf
--
{-
    General
    
    All of the following questions are taken directly from past exam 
    papers. To check your solutions write tests that they behave according 
    to the examples given, plus QuickCheck tests to test correctness.

-}
import Test.QuickCheck
import Data.Char
import Data.List

{- 1.
    (a) Write a function 
            f1 :: [String] -> String 
            
        to concatenate every word in a list that begins with an upper case
        letter. For example,
        
            f1 ["This","Is","not","A","non","Test"] = "ThisIsATest"
            f1 ["noThing","beGins","uPPER"] = ""
            f1 ["Non-words","like","42","get","Dropped"] = "Non-wordsDropped"
            f1 ["An","Empty","Word","","gets","dropped"] = "AnEmptyWord"
            
        Use basic functions, list comprehension, and library functions, 
        but not recursion.

-}
q1input = [["This","Is","not","A","non","Test"],
           ["noThing","beGins","uPPER"],
           ["Non-words","like","42","get","Dropped"],
           ["An","Empty","Word","","gets","dropped"]]

q1output = ["ThisIsATest", "",  "Non-wordsDropped", "AnEmptyWord"]

f1 :: [String] -> String
f1 xs = concat [ x | x <- xs, x /= "" && isUpper (head x)]

testF1 = (map f1 q1input) == q1output

{- (b)
    Write a second function 
        g1 :: [String] -> String 
        
    that behaves like f1, this time using basic functions, recursion, and 
    the library function to append two lists, but not list comprehension 
    or other library functions.
    
-}       

g1 :: [String] -> String
g1 (x:xs) |   not (null x) 
           && isUpper (head x)   = x ++ g1 xs
          |   otherwise          = g1 xs
g1 _ = []          

testG1 = (map g1 q1input) == q1output

prop_q1 :: [String] -> Bool
prop_q1 xs = f1 xs == g1 xs

{- 2.
    (a) Write a function 
            f2 :: String -> Bool 
            
        to verify that every vowel in a string is uppercase. In English, 
        the following characters are the vowels: 'a', 'A', 'e', 'E', 'i', 
        'I', 'o', 'O', 'u', and 'U'. The function should return True for
        strings that have no vowels at all. For example,
        
                f2 "ALL CAPS" == True
                f2 "r3cURsI0n" == True
                f2 [] == True
                f2 "normal text" == False
                
        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}
q2input = ["ALL CAPS", "r3cURsI0n", "", "normal text"]
q2output = [True, True, True, False]

f2 :: String -> Bool
f2 [] = True
f2 xs = and [ isUpper x | x <- xs, isVowel x ]
    where
        isVowel x = elem x "aeiouAEIOU"
        
testF2 = (map f2 q2input) == q2output

{-
    (b) Write a second function 
            g2 :: String -> Bool 
            
        that behaves like f2, this time using basic functions and 
        recursion, but not list comprehension or other library functions.        
-}     
-- modified based on provided solution
g2 :: String -> Bool
g2 []                           = True
g2 (x:xs) | elem x "aeiouAEIOU" = isUpper x && g2 xs
          | otherwise = g2 xs

testG2 = (map g2 q2input) == q2output          

{-
    (c) Write a third function 
            h2 :: String -> Bool 
            
        that also behaves like f2, this time using one or more of the 
        following higher-order library functions:
        
                map :: (a -> b) -> [a] -> [b]
                filter :: (a -> Bool) -> [a] -> [a]
                foldr :: (a -> b -> b) -> b -> [a] -> b
        
        You may also use basic functions, but not list comprehension, 
        other library functions, or recursion.
-}
h2 :: String -> Bool
h2 = foldr (&&) True . map isUpper . filter isVowel
    where
        isVowel x = elem x "aeiouAEIOU"
        
testH2 = (map h2 q2input) == q2output

prop_q2 :: String -> Bool
prop_q2 xs = f2 xs == g2 xs && f2 xs == h2 xs

{-
    3.
    (a) Write a function 
            f3 :: String -> Bool 
            
        to verify that all the digits in a string are equal to or greater 
        than 5. For example,
        
            f3 "normal text" == True
            f3 "number 75" == True
            f3 "" == True
            f3 "17 is a prime" == False
            
        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}
q3input  = ["normal text", "number 75", "", "17 is a prime"]
q3output = [True, True, True, False]

f3 :: String -> Bool
f3 xs = and [ gt5 x | x <- xs ]
    where
        gt5 x | isDigit x  = digitToInt x >= 5
              | otherwise  = (ord x) >= 5
              
testF3 = (map f3 q3input) == q3output

{-
    (b) Write a second function 
            g3 :: String -> Bool 
        that behaves like f3, this time using basic functions and 
        recursion, but not list comprehension or other library functions.
-}              
g3 :: String -> Bool
g3 (x:xs) | isDigit x = digitToInt x >= 5 && f3 xs
          | otherwise = ord x >= 5 && f3 xs
g3 _ = True

testG3 = (map g3 q3input) == q3output

{-
    (c) Write a third function 
            h3 :: String -> Bool 
        
        that also behaves like f3, this time using one or more of the 
        following higher-order library functions:
        
            map :: (a -> b) -> [a] -> [b]
            filter :: (a -> Bool) -> [a] -> [a]
            foldr :: (a -> b -> b) -> b -> [a] -> b

         You may also use basic functions, but not list comprehension, 
         other library functions, or recursion.
-}

h3 :: String -> Bool
h3 = foldr (&&) True . map ge5
    where
        ge5 x | isDigit x = digitToInt x >= 5
              | otherwise = ord x >= 5
              
testH3 = (map h3 q3input) == q3output

prop_q3 :: String -> Bool
prop_q3 xs = f3 xs == g3 xs && g3 xs == h3 xs

{-
    4.
    (a) Write a function 
            f4 :: String -> Bool 
            
        to verify that every punctuation character in a string is a space. 
        A character is punctuation if it is not a letter or a digit. The 
        function should return True for strings that have no punctuation
        characters at all. For example,
        
            f4 "Just two spaces" == True
            f4 "No other punctuation, period." == False
            f4 "No exclamations!" == False
            f4 "What the @#$!?" == False
            f4 "l3tt3rs and d1g1ts 0k" == True
            f4 "NoSpacesAtAllOK" == True
            f4 "" == True

        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
    
-}
q4input = ["Just two spaces",
           "No other punctuation, period.",
           "No exclamations!",
           "What the @#$!?",
           "l3tt3rs and d1g1ts 0k",
           "NoSpacesAtAllOK",
           ""]

q4output = [True, False, False, False, True, True, True]

f4 :: String -> Bool
f4 xs = and [ isSpace x | x <- xs, not (isAlpha x), not (isDigit x) ]

testF4 = (map f4 q4input) == q4output

{-
    (b) Write a second function 
            g4 :: String -> Bool 
        that behaves like f4, this time using basic functions and 
        recursion, but not list comprehension or other library functions.
-}
g4 :: String -> Bool
g4 (x:xs) | isSpace x       = True  && g4 xs
          | isPunctuation x = False && g4 xs
          | otherwise       = g4 xs
    where
        isPunctuation x = not (isAlpha x) && not (isDigit x)
g4 _ = True

testG4 = (map g4 q4input) == q4output

{-
    (c) Write a third function 
            h4 :: String -> Bool
            
        that also behaves like f4, this time using one or more of the 
        following higher-order library functions:

            map :: (a -> b) -> [a] -> [b]
            filter :: (a -> Bool) -> [a] -> [a]
            foldr :: (a -> b -> b) -> b -> [a] -> b

        You may also use basic functions, but not list comprehension, 
        other library functions, or recursion.        
-}
h4 :: String -> Bool
h4 = foldr (&&) True . map isSpace . filter isPunctuation
    where
        isPunctuation x = not( isAlpha x || isDigit x )
        
testH4 = (map h4 q4input) == q4output

prop_q4 :: String -> Bool
prop_q4 xs = f4 xs == g4 xs && g4 xs == h4 xs

{-
    5.
    (a) Write a function 
            f5 :: String -> Bool 
            
        that takes a string, and returns True if every digit in the string
        is even. Any characters in the string that are not digits should 
        be ignored. For example,

            f5 "246" == True
            f5 "2467" == False
            f5 "x4y2z" == True
            f5 "abc12" == False

        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}
q5input = ["246", "2467", "x4y2z", "abc12"]
q5output = [True, False, True, False]

f5 :: String -> Bool
f5 xs = and [ even (digitToInt x) | x <- xs, isDigit x]

testF5 = (map f5 q5input) == q5output

{-
    (b) Write a second function 
            g5 :: String -> Bool 
            
        that behaves like f5, this time using basic functions and 
        recursion, but not list comprehension or other library functions.
-}
g5 :: String -> Bool
g5 (x:xs) | isDigit x   = even (digitToInt x) && g5 xs
          | otherwise   = g5 xs
g5 _ = True

testG5 = (map g5 q5input) == q5output

{-
    (c) Write a third function 
            h5 :: String -> Bool 
            
        that also behaves like f5, this time using one or more of the 
        following higher-order library functions:
        
            map :: (a -> b) -> [a] -> [b]
            filter :: (a -> Bool) -> [a] -> [a]
            foldr :: (a -> b -> b) -> b -> [a] -> b

        You may also use basic functions, but not list comprehension, 
        other library functions, or recursion.
-}          
h5 :: String -> Bool
h5 = foldr (&&) True . map (even . digitToInt) . filter isDigit

testH5 = (map h5 q5input) == q5output

prop_q5 :: String -> Bool
prop_q5 xs = f5 xs == g5 xs && g5 xs == h5 xs


        
        
