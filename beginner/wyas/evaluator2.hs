module Main where
{-
    Chapter 3
        Functions added:
            eval        -- evaluate straight forward LispVal's
                        --    String, Number, Bool, and quoted Lists
            
        Functions modified:
            readExpr    -- altered to return a LispVal vs a String
            main        -- altered to now evaluate and display expressions
            
    Notes:
        to run in ghci, load the file and then type
            :main <args>
            
        For example,
            *Main> :main "'atom"
            atom
            *Main> :main "\"a string \""
            "a string "
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
-- '>>=' is a Monad sequencer operator; first 'getArgs' is executed
-- to grab command line arguments and then the retrieved values are
-- immediately passed to the composite function
--      print . eval . readExpr . head
-- which can be read as
--      print(eval(readExpr (head args)))
-- parentheses indicate evaluation order, just as in mathematics
-- with inner operations executed first 
-- i.e. (head args) is evaluated with the result being passed to
--      readExpr - whose results are passed to
--      eval     - whose results are passed to 
--      print    - which displays the results of eval on the screen
--
main = getArgs >>= print .  eval . readExpr .  head
        
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"     
        
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err  -> String $ "No match: " ++ show err
    Right val  -> val
    
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
        
showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "."
                                     ++ showVal tail ++ ")"
                                     
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Evaluate a LispVal
--      for String, Number, Bool or Atom LispVal types, we
--      just want to return the type itself
--      i.e. for Lisp, in these cases, the 'code' and the 'data'
--           are identical
--      @ notation let's us assign a 'name' to an entire
--        pattern so we can reference it in part or as a whole
--
eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _ )              = val
eval (List [Atom "quote", val]) = val       -- nested pattern match





        