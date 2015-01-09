module Main where
{-
    Chapter 3
        Functions added:
            apply          - apply a function to its arguments
            primitives     - list of valid operations
            numericBinop   - function to execute a given valid operation
            unpackNum      - convert a LispVal number to an Integer
            
        Functions modified:
            eval    -- added clause to handle function application
            
    
    Example
    
        *Main> :main "(+ 2 2)"
        4
        *Main> :main "(+ 2 (-4 1))"
        2
        *Main> :main "(+ 2 (- 4 1))"
        5
        *Main> :main "(- (+ 4 6 3) 3 5 2)"
        3
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

eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _ )              = val
eval (List [Atom "quote", val]) = val 
-- match against the list 'cons' operator (:)
eval (List (Atom func : args))  = apply func $ map eval args

-- if possible, evaluate the given function by applying it to the
-- given arguments; return  a LispVal of 'False' if it's not possible
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- key-value pairs for valid operations
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+",         numericBinop (+))
             ,("-",         numericBinop (-))
             ,("*",         numericBinop (*))
             ,("/",         numericBinop div)
             ,("mod",       numericBinop mod)
             ,("quotient",  numericBinop quot)
             ,("remainder", numericBinop rem)]

-- create the required function from the given operator and parameters            
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- convert a LispVal number to an integer
-- for now, if no valid number found, return zero
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = read n :: [(Integer, String)]
                       in if null parsed 
                          then 0 
                          else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0





        