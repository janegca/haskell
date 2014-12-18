-- Informatics 1 - Functional Programming 
-- Tutorial 4 - Screen Scrapper
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/tutorial4.zip
{-
    Basic Screen Scraper
    
    A "screen scraper" is a tool used to extract data from web sites, by
    looking at their source. In this exercise, you will write one of the 
    most hated screen scrapers: one that extracts email addresses.
    
    Why is it hated? Because people use screen scrapers like that to 
    collect email addresses to send spam to. However, in this exercise we
    will show you a useful purpose of the email screen scraper! 
    
    We are going to be extracting names and emails from web pages written 
    in HTML (HyperText Markup Language). For instance, from the following
    HTML:
    
    <html>
        <head>
            <title>FP: Tutorial 4</title>
        </head>
        <body>
            <h1>A Boring test page</h1>
            <h2>for tutorial 4</h2>
            <a href="http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/">
            FP Website</a><br>
            <b>Lecturer:</b> <a href="mailto:dts@inf.ed.ac.uk">
            Don Sannella</a><br>
            <b>TA:</b> <a href="mailto:m.k.lehtinen@sms.ed.ac.uk ">
            Karoliina Lehtinen</a>
        </body>
    </html>
    
    We are going to extract a list of the "<a>" elements, which contain 
    URLs (Uniform Resource Locators). If a URL begins with http: it is an
    address of a web page; if it begins with mailto: the rest of it is an 
    email address. For the document above, here is the list of links 
    (each one contains some extra data at the end, which is an artifact 
    of the technique we use):
    
    ["http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">
      FP Website</a><br><b>Lecturer:</b> "
    ,"mailto:dts@inf.ed.ac.uk\">\Don Sannella</a><br><b>TA:</b> "
    ,"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body>
      </html>"]
      
    From this list, we will in turn extract a list of names and email 
    addresses:
        [("Don Sannella","dts@inf.ed.ac.uk"),
         ("Karoliina Lehtinen","m.k.lehtinen@ed.ac.uk")]
         
    This file contains the test html-document and the lists 
    above: testHTML, testLinks, and testAddrBook.
    
    Notice that the type of testLinks is [Link] and the type of 
    testAddrBook is [(Name,Email)]. In other words: testLinks is a list of
    Links, and testAddrBook is a list of tuples containing both a Name and
    an Email. These appear to be new types which we have not encountered 
    before, but if you look in the file you will find the following type 
    expressions:

        type Link  = String
        type Name  = String
        type Email = String
        type HTML  = String
        type URL   = String

    These type declarations simply define aliases for the very familiar 
    type String. Aliases are not strictly necessary, but they make your 
    program more readable.

-}

import Data.List
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

{- 1. Write a function 
         sameString :: String -> String -> Bool 
         
      that returns True when two strings are the same, but ignores 
      whether a letter is in upper- or lowercase. For example:
      
        *Main> sameString "HeLLo" "HElLo"
        True
        *Main> sameString "Hello" "Hi there"
        False
-}
sameString :: String -> String -> Bool
sameString xs ys = map toLower xs == map toLower ys

{- 2. Write a function 
        prefix :: String -> String -> Bool 
    
    that checks whether the first string is a prefix of the second, like
    the library function isPrefixOf that you used before, but this time it
    should be case-insensitive.
    
        *Main> prefix "bc" "abCDE"
        False
        *Main> prefix "Bc" "bCDE"
        True
    
    Check your function using the predefined test property prop_prefix.

-}
prefix :: String -> String -> Bool
prefix sstr str = sameString sstr ( take (length sstr) str )

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		              prefix substr (map toUpper str)
                          where
                            substr  =  take n str

{- 3. Write the function contains as in tutorial 2, but case-insensitive.
      For example:
      
        *Main> contains "abcde" "bd"
        False
        *Main> contains "abCDe" "Bc"
        True
        
      Write a test property 
            prop_contains :: String -> Int -> Int -> Bool 
      
      to test your contains function. You can take inspiration from 
      prop_prefix.

-}
contains :: String -> String -> Bool
contains [] _     = False
contains [x] sstr = isPrefixOf sstr [x]
contains str sstr | prefix sstr str  = True
                  | otherwise        = contains (tail str) sstr
        
-- provided solution        
contains' :: String -> String -> Bool                  
contains' str sstr = or [ prefix sstr s | s <- tails str ]                   

-- this test fails for both definitions (the one provided in the
-- solution file also fails on )
prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m = contains substr (map toLower str) &&
		                contains substr (map toUpper str)
                          where
                            substr  =  take n (drop m str)

{- 4.
    (a) Write a case-insensitive function 
            takeUntil :: String -> String -> String 
            
        that returns the contents of the second string before the first 
        occurrence of the first string. If the second string does not 
        contain the first as a substring, return the whole string. E.g.:
        
            *Main> takeUntil "cd" "abcdef"
            "ab"

    (b) Write a case-insensitive function 
            dropUntil :: String -> String -> String 
        
        that returns the contents of the second string after the first 
        occurrence of the first string. If the second string does not 
        contain the first as a substring, return the empty string. E.g.:
        
            *Main> dropUntil "cd" "abcdef"
            "ef"
-}
takeUntil :: String -> String -> String
takeUntil sstr str@(x:xs) 
    | prefix sstr str = []
    | otherwise = x : takeUntil sstr xs
takeUntil _ _ = []

dropUntil :: String -> String -> String
dropUntil sstr str@(x:xs) 
    | prefix sstr str = drop (length sstr - 1) xs
    | otherwise       = dropUntil sstr xs
dropUntil _ _ = []

{- 5.
    (a) Write a case-insensitive function 
            split :: String -> String -> [String] 
        
        that divides the second argument at every occurrence of the first, 
        returning the results as a list. The result should not include the 
        separator. For example:
        
            *Main> split "," "comma,separated,string"
            ["comma","separated","string"]
            *Main> split "the" "to thE WINNER the spoils!"
            ["to "," WINNER "," spoils!"]
            *Main> split "end" "this is not the end"
            ["this is not the ",""]
            
        Your function should return an error if the first argument, the 
        separator string, is an empty list. You will find your functions 
        takeUntil and dropUntil useful here.
        
    (b) Write a function 
            reconstruct :: String -> [String] -> String 
        
        that reverses the result of split. That is, it should take a 
        string and a list of strings, and put the list of strings back 
        together into one string, with the first string everywhere in 
        between (but not at the start or at the end).
        [Note: you can use Data.List 'intercalate' function to do this]
        
    (c) Look at the predefined test function prop_split and explain what 
        it does. Use it to test your split function.
        
        The function combines a given char and string into a new 
        separator string, uses it to split and reconstruct a string
        and then check that the reconstructed string is equal to the 
        original.
        
    
-}
split :: String -> String -> [String]
split "" _     = error("separator cannot be empty string")
split sep str 
    | contains str sep = takeUntil sep str : split sep (dropUntil sep str)
    | otherwise        = [str]  -- modified after seeing provided solution

reconstruct :: String -> [String] -> String
reconstruct sep [x]    = x
reconstruct sep (x:xs) = x ++ sep ++ reconstruct sep xs

-- provided solution included foldr1 example
reconstruct' :: String -> [String] -> String
reconstruct' sep = foldr1 f
    where
        f xs ys = xs ++ sep ++ ys

-- Data.List library function will do the same
reconstruct'' :: String -> [String] -> String
reconstruct'' sep = intercalate sep 

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

{- 6. Use your function split to write a function 
            linksFromHTML :: HTML -> [Link]. 
      
      You can assume that a link begins with the string <a href=". Don't 
      include this separator in the results, and don't include the stuff
      in the HTML that precedes the first link. Example:

        *Main> linksFromHTML testHTML
        ["http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">
                FP Website</a><br><b>Lecturer:</b> ",
        "mailto:dts@inf.ed.ac.uk\">\Don Sannella</a><br><b>TA:</b> ",
        "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>"]

      Note: to include the character " in a string, precede it with a 
            backslash (\), as \".
        
      Use testLinksFromHTML to test your function on the given sample
      data. Note that this test does not require QuickCheck, since it does
      not depend on randomly generated input.
    
-}
linksFromHTML :: HTML -> [Link]
linksFromHTML = split ref . dropUntil ref
    where 
        ref = "<a href=\""
        
-- provided solution
linksFromHTML' :: HTML -> [Link]
linksFromHTML' doc = tail (split "<a href=\"" doc)

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


{- 7. Write a function 
            takeEmails :: [Link] -> [Link] 
            
      which takes just the email addresses from a list of links given by
      linksFromHTML. Example:
        *Main> takeEmails testLinks
        ["mailto:dts@inf.ed.ac.uk\">\Don Sannella</a><br><b>TA:</b> ",
        "mailto:m.k.lehtinen@ed.ac.uk\">Karoliina Lehtinen</a></body></html>"]

-}
takeEmails :: [Link] -> [Link]
takeEmails = filter ("mailto" `prefix`)

{- 8. Write a function 
        link2pair :: Link -> (Name, Email) 
        
      which converts a mailto link into a pair consisting of a name and 
      the corresponding email address. The name is the part of the
      link between the <a href="..."> and </a> tags; the email address is 
      the part in the quotes after mailto:. Add an appropriate error 
      message if the link isn't a mailto: link. Example:
      
      *Main> link2pair "mailto:john@smith.co.uk\">John</a>"
      ("John","john@smith.co.uk")

-}
link2pair :: Link -> (Name, Email)
link2pair link 
    | prefix "mailto" link = (takeUntil "<" . dropUntil ">"  $ link,
                              takeUntil "\"" . dropUntil ":" $ link )
    | otherwise            = error("not an email address")

{- 9. Combine your functions linksFromHTML, takeEmails and link2pair to 
      write a function
            emailsFromHTML :: HTML -> [(Name, Email)] 
            
      that extracts all mailto links from a web page, turns them into 
      (Name, Email) pairs, and then removes duplicates from that list.
      Example:
        *Main> emailsFromHTML testHTML
        [("Don Sannella","dts@inf.ed.ac.uk"),
        ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]
      
      Note: the library function nub :: [a] -> [a] removes duplicates 
            from a list.
        
      You can test your function with testEmailsFromHTML.

-}
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML = nub . map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook

{-
    Test your function by calling:
    
        emailsFromURL testURL
        
    This will retrieve email addresses from a test page at
    "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"
    
    Try some other websites
-}
{- 10.
    Write a function 
        findEmail :: Name -> [(Name,Email)] -> [(Name,Email)] 
    
    which given (part of) a name and a list of (Name,Email) pairs, 
    returns a list of those pairs which match the name. Example:
    
        *Main> findEmail "Karoliina" testAddrBook
        [("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]
        *Main> findEmail "San" testAddrBook
        [("Don Sannella","dts@inf.ed.ac.uk")]
        *Main> findEmail "Fred" testAddrBook
        []
-}
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail str addrs = 
    [(name, addr) | (name, addr) <- addrs, contains name str]

{- 11.
    Define the function 
            emailsByNameFromHTML :: HTML -> Name -> [(Name, Email)]. 
    
    This function should take an HTML string and (part of) a name, and 
    return all (Name,Email) pairs which match the name.
    
        *Main> emailsByNameFromHTML testHTML "Karoliina"
        [("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]
-}
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name . emailsFromHTML $ html


-- Optional Material

{- 12.
    Rewrite the function 
            ppAddrBook :: [(Name,Email)] -> String 
    
    so that it lines up the names and email addresses in two separate 
    columns. For example:
    
        *Main> putStr (ppAddrBook testAddrBook)
        Sannella, Don       dts@inf.ed.ac.uk
        Lehtinen, Karoliina m.k.lehtinen@sms.ed.ac.uk
    
    You will find, in general, that some names are listed in "surname, 
    first name" format and some are given in the regular "first name 
    surname" format. Make sure your function can cope with both formats.
    
    Note: The function putStr takes a string and prints it to the screen,
          which involves turning newline characters '\n' into actual new 
          lines.
-}
-- based on provided solution
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [  take max (format(name) ++ repeat ' ') 
                             ++ email 
                           | (name,email) <- addr ]
    where
        max = 2 + maximum (map (length . fst) addr)
        
        format name | contains name " " = dropUntil " " name
                                       ++ ", " 
                                       ++ takeUntil " " name
                    | otherwise = name
         
