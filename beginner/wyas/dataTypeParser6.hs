module Main where

{- Exercise 6

   Add a Float constructor to LispVal and write a parser to support
   R5RS syntax for decimals.
   
-}


import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

import Numeric (readHex, readOct, readInt, readFloat)
import Data.Char (digitToInt, toLower)


data LispVal = Atom        String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
             | Float       Double           -- added for Exercise 6
             | Number      Integer
             | String      String
             | Character   Char             -- added for Exercise 5             
             | Bool        Bool
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

             
-- revised in Exercise 2 and 3
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x                             
         
escapedChars :: Parser Char
escapedChars = do char '\\' 
                  x <- oneOf "\\\"nrt" 
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'
                    
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

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space") 
         <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ Character $ case value of
        "space"   -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)
                
parseFloat :: Parser LispVal
parseFloat = do wnum <- many1 digit
                char '.'
                frac <- many1 digit
                let num = wnum ++ "." ++ frac
                return $ Float (fst . head $ readFloat num)

-- parser to accept either a parser String or Number
parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString    
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         
         
        
        