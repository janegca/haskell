module Main where
{-
    Chapter 3
        Functions added:
            showVal         string representation of LispVal data
            unwordsList
            
        Instance added:
            Show
            
    Example output:
    
        *Main> readExpr "(1 2 2)"
        "Found (1 2 2)"
        
        *Main> readExpr "'(1 3 (\" this \" \"one \"))"
        "Found (quote (1 3 (\" this \" \"one \")))"
        *Main>     
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

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))
        
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"     
        
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err  -> "No match: " ++ show err
    Right val  -> "Found "     ++ show val
    
spaces :: Parser ()
spaces = skipMany1 space
        
parseString :: Parser LispVal
parseString = do 
                char '"'                 -- start of string
                x <- many (noneOf "\"")  -- everything between quotes
                char '"'                 -- end of string
                return $ String x        -- return 'x' as a LispVal
         
parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol      
                rest  <- many (letter <|> digit <|> symbol)
                let atom = [first] ++ rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            otherwise -> Atom atom
                            
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

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

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x
        
-- Creates a custom 'string' to represent a LispVal        
-- use pattern matching to 'deconsruct' a LispVal type
-- each left-hand clause matches one of the LispVal constructors
-- the right-hand clause tells what do do if the input matches the
-- the left-hand clause
--         
showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "."
                                     ++ showVal tail ++ ")"
                                     
-- convert a list of LispVal's to string and concatenate them                              
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- tell compiler LispVal belongs to the 'Show' type class and instruct
-- on which function to use to convert a LispVal to a String
instance Show LispVal where show = showVal



        