module Main where

{-
    To compile:
    
    ghc −package parsec −o simple_parser simpleParser1.hs
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

-- read and expression by passing the 'input', the 'lisp' parser
-- and our 'symbol' action to the 'parse' function
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"

{-
    Example usage
    
    C:\Users\...\beginner\wyas>simple_parser $
    Found value

    C:\Users\Jane\...\beginner\wyas>simple_parser a
    No match: "lisp" (line 1, column 1):
    unexpected "a"  

    C:\Users\...\beginner\wyas>simple_parser "  %"
    No match: "lisp" (line 1, column 1):
    unexpected " "    

-}