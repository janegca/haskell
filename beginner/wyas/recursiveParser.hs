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
    deriving Show
             
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
    Left  err  -> "No match: " ++ show err
    Right val  ->  show val         --  "Found value"
   
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

-- parase a series of expressions separated by whitespace and
-- then apply the List constructor to the resulting Parser String
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- represents Scheme from (a b . c) which stores all bu the last
-- list element; placing it in a separate field
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces  -- end parsed value with a space
    tail <- char '.' >> spaces >> parseExpr   -- sequence of actions
    return $ DottedList head tail
    
parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''           -- escaped single quote character
        x <- parseExpr
        return $ List [Atom "quote", x]

-- parser to accept either a parser String or Number
-- the 'try' combinator attempts to run the specified parser
-- if it fails, it 'backs up' to the previous state
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x

{-
    Examples
    
    *Main> readExpr "(a test)"
    "List [Atom \"a\",Atom \"test\"]"
    
    *Main> readExpr "(a (nested) test)"
    "List [Atom \"a\",List [Atom \"nested\"],Atom \"test\"]"
    
    *Main> readExpr "(a (dotted . list) test)"
    "List [Atom \"a\"
          ,DottedList [Atom \"dotted\"] (Atom \"list\")
          ,Atom \"test\"]"
    
    *Main> readExpr "(a '(quoted (dotted . list)) test)"
    "List [Atom \"a\"
          ,List [Atom \"quote\"
                ,List [Atom \"quoted\"
                      ,DottedList [Atom \"dotted\"] (Atom \"list\")]
                ]
          ,Atom \"test\"]"
    
    *Main> readExpr "(a ’(imbalanced parens)"
    "No match: \"lisp\" (line 1, column 4):\nunexpected \"\\8217\"\nexpecting letter, \"\\\"\", digit, \"'\", \"(\" or \".\""
    *Main>     

-}                
                                                       
