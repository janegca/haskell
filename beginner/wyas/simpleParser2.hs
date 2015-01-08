module Main where

{-
    Fix simpleParser1 so it can handle WhiteSpace
    
    To compile:
    
    ghc −package parsec −o simple_parser simpleParser2.hs
-}

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))

-- 'action' to identify symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

-- read an expression by passing the 'input', the 'lisp' parser
-- and our 'symbol' action to the 'parse' function
--
-- MODIFIED: we now want to parse using two Parser actions
--          spaces (defined below) and symbol
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"

-- parser to recognize, and skip, white space
-- combines two Parser actions: space and skipMany1
spaces :: Parser ()
spaces = skipMany1 space

{-
    Will now recognize one or more spaces BUT no longer recognized
    a single symbol
    
    C:\Users\...\beginner\wyas>simple_parser "   %"
    Found value

    C:\Users\...\beginner\wyas>simple_parser %
    No match: "lisp" (line 1, column 1):
    unexpected "%"
    expecting space

    C:\Users\...\beginner\wyas>simple_parser "   abc"
    No match: "lisp" (line 1, column 4):
    unexpected "a"
    expecting space    

-}    

    