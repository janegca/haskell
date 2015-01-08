module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             
main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))
                     
             
-- 'action' to identify symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"          

-- read an expression by passing the 'input', the 'lisp' parser
-- and our 'symbol' action to the 'parse' function
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _  -> "Found value"
   
-- parser to recognize, and skip, white space
-- combines two Parser actions: space and skipMany1
spaces :: Parser ()
spaces = skipMany1 space

             
-- a 'string' is a double quote mark followed by any number
-- of non-quote characters followed by a closing quote mark             
parseString :: Parser LispVal
parseString = do 
                char '"'                 -- start of string
                x <- many (noneOf "\"")  -- everything between quotes
                char '"'                 -- end of string
                return $ String x        -- return 'x' as a LispVal
         
-- an 'atom' is a letter or symbol followed by any number
-- of letters, digits or symbols        
-- [NOTE:  
--      <|>     Parsec 'choice' operator
--                  tries first parser, if that fails, tries next, etc.
-- ] 
parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol      
                rest  <- many (letter <|> digit <|> symbol)
                let atom = [first] ++ rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            otherwise -> Atom atom
                            
-- parser for numbers (digits)      
-- [Notes:
--      digit and many1 are Parsec functions
--      many1 digit  
--          reads digits, returning them as a Parser String
--      liftM (Number . read)
--          read   - converts string to Integer
--          Number - LispVal constructor of type Integer
--          liftM  - operate on the value 'inside' the monad
--                   ie 'inside' the Parser String returned by 'many1'
--                   requires import of Monad
-- ]                      
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- parser to accept either a parser String or Number
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
                                                       
{-
    The parser will now accept any number, string or symbol but not
    other strings:

    C:\Users\...\beginner\wyas>simple_parser "\"this is a string\""
    Found value

    C:\Users\...\beginner\wyas>simple_parser 25
    Found value

    C:\Users\...\beginner\wyas>simple_parser symbol
    Found value

    C:\Users\...\beginner\wyas>simple_parser (symbol)
    No match: "lisp" (line 1, column 1):
    unexpected "("
    expecting letter, "\"" or digit

    C:\Users\...\beginner\wyas>simple_parser  "(symbol)"
    No match: "lisp" (line 1, column 1):
    unexpected "("
    expecting letter, "\"" or digit

-}

-- EXERCISES -------------------------------------------------------------
-- with some help from
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Answers
--
-- 1a. re-write parseNumber with 'do' notation
parseNumber' :: Parser LispVal
parseNumber' = do num <- many1 digit
                  return $ Number (read num)
                  
-- 1b. re-write parseNumber with explicit sequencing using '>>='
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number . read

-- 2. re-write parseString to handle escaped quotes; write another parser
--    to handle the \" character
escapedChar :: Parser Char
escapedChar = do char '\\'          -- backslash
                 x <- oneOf "\\\""  -- backslash or double quote
                 return x           -- return the escaped character
                 
parseString' :: Parser LispVal
parseString' = do 
                char '"'                 -- start of string
                x <- many  $  escapedChar 
                          <|> noneOf "\"\\" 
                char '"'                 -- end of string
                return $ String x        -- return 'x' as a LispVal
         
{-
    To test the above, modify 'readExpr' to use the new parsers
    and then pass in some values
    
    *Main> readExpr "use \"nested string\" for test"
    "Found value"
    
    *Main> readExpr "abc 19 in 2 days"
    "Found value"
        
-}
   
-- 3. Modify previous exercise to handle \n \r \t and any other
--    desired characters
escapedChar' :: Parser Char
escapedChar' = do char '\\'          
                  x <- oneOf "\\\"nrt" 
                  return $ case x of
                     '\\'    -> x
                     '"'     -> x
                     'n'     -> '\n'
                     'r'     -> '\r'
                     't'     -> '\t'
                     
-- 4. see dataTypeParser4.hs
-- 5. see dataTypeParser5.hs