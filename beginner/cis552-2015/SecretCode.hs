{-# OPTIONS -Wall #-}
module SecretCode where

{-
    CIS 552: Advanced Programming (2015)
    Lecture 3 - Higher-Order Programming Patterns
        Practise in recognizing patterns and converting
        them to higher order functions
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/SecretCode.html

-}
{-
    OK, we're going to write a Haskell program to encode and decode text 
    files using a secret code. We'll call it the Brown Fox code. 
    Here's how it works:

    - Replace each letter according to the following correspondence:

            "abcdefghijklmnopqrstuvwxyz"
        to  "thequickbrownfxjmpsvlazydg"

      But leave any non-letter characters alone.

    - Then reverse the order of the lines in the file.
-}
import Data.Char
import Data.Maybe
import Test.HUnit

-- first, make a lookup lists for the pairings
code :: [(Char,Char)]
code =    (zip ['a' .. 'z'] cypher)
       ++ (zip ['A' .. 'Z'] (map toUpper cypher))
  where 
    cypher :: String
    cypher = "thequickbrownfxjmpsvlazydg"
    
-- encode a character using the Prelude 'lookup' function
-- and the 'fromMaybe' function
encodeChar :: Char -> Char
encodeChar c = fromMaybe c (lookup c code)

-- test the code so far
testEncodeChar :: IO Counts
testEncodeChar = runTestTT $ TestList [ encodeChar 'a' ~?= 't', 
                                        encodeChar '.' ~?= '.']

{-
    *SecretCode> testEncodeChar

    Cases: 2  Tried: 0  Errors: 0  Failures: 0
    Cases: 2  Tried: 1  Errors: 0  Failures: 0
                                              
    Cases: 2  Tried: 2  Errors: 0  Failures: 0
    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
    *SecretCode> 
-}                

-- and now we can use the 'map' abstraction pattern to encode
-- an entire line of text
encodeLine :: String -> String
encodeLine x = map encodeChar x

testEncodeLine :: IO Counts
testEncodeLine = runTestTT 
               $ TestList [ encodeLine "abc defgh" ~?= "the quick"]

{-
    *SecretCode> testEncodeLine

    Cases: 1  Tried: 0  Errors: 0  Failures: 0
                                              
    Cases: 1  Tried: 1  Errors: 0  Failures: 0
    Counts {cases = 1, tried = 1, errors = 0, failures = 0}
    *SecretCode> 

-}       

-- and finally, we want to encode an entire file, reversing the lines
encodeContent :: String -> String
encodeContent = unlines . reverse . map encodeLine . lines

-- here's how we read in a file
encodeFile :: FilePath -> IO ()
encodeFile f = do fcontents <- readFile f
                  writeFile (f ++ ".code") (encodeContent fcontents)
        
-- make our program executable        
main :: IO ()
main = do putStrLn "What file shall I encode?"
          fn <- getLine
          encodeFile fn
          putStrLn "All done!"

          

    
    
