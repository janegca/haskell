module Main where

{-
    Exercise 1
    
    Add support for the backquote syntactic sugar: the Scheme standard
    details what it should expand into (quasiquote/unquote).
    
    Notes from Scheme
    http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html
    
    "Backquote or quasiquote expressions are useful for constructing
    a list or vector structure when most but not all of the desired 
    structure is known in advance. If no commas appear within the 
    <qq template>, the result of evaluating `<qq template> is equivalent 
    to the result of evaluating '<qq template>. If a comma appears within 
    the <qq template>, however, the expression following the comma is 
    evaluated (``unquoted'') and its result is inserted into the structure 
    instead of the comma and the expression."  (further references to
    '@' symbol and what follows ... not implemented)
    
    Two parsers are required, one to recognize the 'backquote' and
    a second to recognize a comma (,) or 'unquote'

-}

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
        
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
        char '`'           -- backquote character
        x <- parseExpr
        return $ List [Atom "qq", x]
        
parseUnQuote :: Parser LispVal
parseUnQuote = do
     char ','
     x <- parseExpr
     return $ List [Atom "unquote", x]        
        

-- parser to accept either a parser String or Number
-- the 'try' combinator attempts to run the specified parser
-- if it fails, it 'backs up' to the previous state
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x                                                       
{-

    *Main> readExpr "`(list ,(+ 1 2) 4)"
    "List [Atom \"qq\"
          ,List [Atom \"list\"
                ,List [Atom \"unquote\"
                      ,List [Atom \"+\"
                            ,Number 1,Number 2]
                      ]
                ,Number 4]
          ]"
    
    *Main> readExpr "(let ((name 'a)) `(list ,name ',name))"
    "List [Atom \"let\"
          ,List [List [Atom \"name\",List [Atom \"quote\",Atom \"a\"]]]
          ,List [Atom \"qq\"
                ,List [Atom \"list\"
                      ,List [Atom \"unquote\",Atom \"name\"]
                      ,List [Atom \"quote\"
                            ,List [Atom \"unquote\",Atom \"name\"]
                            ]
                      ]
                ]
          ]"
-}