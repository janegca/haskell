module Main where

{- Exercise 4
    see
    https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Answers

    Change 'parseNumber' to support the 'Scheme Standard for Different
    Bases'. You may find 'readOct' and 'readHex' helpful.

    Notes:
         In Scheme, numbers may be written with following prefixes
             #b      binary
             #d      decimal
             #h      hexidecimal
             #o      octal

         If there is no radix prefix, numbers are assumed to be decimal.
         
    The solution to this exercise requires a series of code modifications:
    
    1. Alter the 'symbol' action and remove the pound symbol as it is now
       required to recognize various number bases.
    2. parseAtom - can no longer see "#t" and "#f"  as atoms, need
                   to define a specific Bool parser, add it to
                   parseExpr and clean up parseAtom
    3. Create individual parsers for each number base and functions
       to convert them to a LispVal number which has type Integer.
       This involves a new 'try' feature which was not covered in the
       example definitions.
       
    Example output from readExpr (modified to show LispVal)
    
        *Main> readExpr "2015"
        "Number 2015"
        *Main> readExpr "#d2015"
        "Number 2015"
        *Main> readExpr "#x7DF"
        "Number 2015"
        *Main> readExpr "#o3737"
        "Number 2015"
        *Main> readExpr "#b11111011111"
        "Number 2015"
        *Main>     
-}


import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

import Numeric (readHex, readOct, readInt)
import Data.Char (digitToInt)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
    deriving Show
             
main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))
                     
-- 'action' to identify symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"          

-- read an expression by passing the 'input', the 'lisp' parser
-- and our 'symbol' action to the 'parse' function
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right r  -> show r -- "Found value"
   
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
                return $ Atom atom
                            
parseBool :: Parser LispVal
parseBool = do
    char '#'
    (    (char 't' >> return (Bool True)) 
     <|> (char 'f' >> return (Bool False)) )                   
                            
-- parser for numbers (digits)      
parseNumber :: Parser LispVal
parseNumber =  parseDefaultDigit 
           <|> parseDecimal
           <|> parseHex
           <|> parseOct
           <|> parseBin

-- parse digits with no prefix, assumed to be decimal digits
parseDefaultDigit :: Parser LispVal
parseDefaultDigit = many1 digit >>= return . Number . read

-- parse decimal digits
parseDecimal :: Parser LispVal
parseDecimal = do try $ string "#d"
                  num <- many1 digit
                  return $ Number (read num)
                                    
-- parse hexidecimal digits
parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              num <- many1 hexDigit
              return $ Number (hex2dig num)
              
parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              num <- many1 octDigit
              return $ Number (oct2dig num)              
              
parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              num <- many1 (oneOf "10") 
              return $ Number (bin2dig num)

-- Note: readHex, readOct and readBin all return a list with a single tuple
--       the first element of the tuple is the hex number converted
--       to decimal
--       Example:  readHex "A9" ==> [(169,"")]
--       Returns an empty list if the String is not a valid number
hex2dig :: (Num a, Eq a) => String -> a              
hex2dig x = fst $ readHex x !! 0          

oct2dig :: (Num a, Eq a) => String -> a
oct2dig x = fst $ readOct x !! 0    

bin2dig :: String -> Integer
bin2dig x = fst $ readBin x !! 0

readBin :: ReadS Integer
readBin = readInt 2 (`elem` "01") digitToInt

-- parser to accept either a parser String or Number
parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseBool
                            
                            

                            