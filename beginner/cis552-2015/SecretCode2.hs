{-# OPTIONS -Wall #-}
module SecretCode2 where

{-
    CIS 552: Advanced Programming (2015)
    Lecture 3 - Higher-Order Programming Patterns
        Practise in recognizing patterns and converting
        them to higher order functions
        
        Refactored SecretCode to handle both
        encoding and decoding a file.
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/SecretCode.html

-}
import Data.Char (toUpper)
import Data.Maybe
import Data.List (isSuffixOf)
import Test.HUnit

-- Note: provided solution used 'Code' for type name; had used 'Table'
type Code = [(Char,Char)]  

code :: Code
code =    (zip ['a' .. 'z'] cypher)
       ++ (zip ['A' .. 'Z'] (map toUpper cypher))
  where 
    cypher :: String
    cypher = "thequickbrownfxjmpsvlazydg"
    
-- Note: name used in provided solution; originally had 'revCode'    
decode :: Code
decode = map (\(a,b) -> (b,a)) code

-- alternative from provided solution
decode' :: Code
decode' = map swap code 
    where swap (x,y) = (y,x)

codeChar :: Code -> Char -> Char
codeChar tbl c = fromMaybe c (lookup c tbl)

codeLine :: Code -> String -> String
codeLine tbl xs = map (codeChar tbl) xs

codeFile :: Code -> String -> String
codeFile tbl = unlines . reverse . map (codeLine tbl) . lines

processFile :: FilePath -> String -> Code -> IO ()
processFile fp ext tbl = do fcontents <- readFile fp
                            writeFile (fp ++ ext) 
                                (codeFile tbl fcontents)
                                
-- tests
testCodeChar :: IO Counts
testCodeChar = runTestTT $ TestList [ codeChar code 'a' ~?= 't', 
                                      codeChar code '.' ~?= '.',
                                      codeChar decode 't' ~?= 'a']
                                      
testCodeLine :: IO Counts
testCodeLine = runTestTT 
               $ TestList [ codeLine code
                                      "abc defgh" ~?= "the quick",
                            codeLine decode
                                     "the quick" ~?= "abc defgh"]
                                      
main :: IO ()
main = do putStrLn "Enter the name of the file to be processed."
          fp <- getLine

          -- establish code table and file extension to be used
          -- (Note: use of 'isSuffixOf' came from provided solution)
          let (tbl, ext) = if isSuffixOf ".code" fp
                           then (decode, ".decode")
                           else (code, ".code")  
                    
          processFile fp ext tbl
          putStrLn "All done!"
          
          

