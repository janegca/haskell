module Main where
{-
    Chapter 5
        Adding boolean operators, conditionals, and some basic string 
        operations.
        
        Functions modified:
            primitives          -- added function primitives
            
        Functions added:
            boolBinop           -- take a primitive fn with 2 args
            numBoolBinop           and pass it on to the correct
            strBoolBinop           unpacker
            boolBoolBinop
            unpackStr
            unpackBool
            
    Examples
    
        *Main> :main "(< 2 3)"
        #t
        *Main> :main "(> 2 3)"
        #f
        *Main> :main "(>= 3 3)"
        #t
        *Main> :main "(string=? \"test\"  \"test\")"
        #t
        *Main> :main "(string<? \"abc\" \"bba\")"
        #t
        *Main>     
            
-}
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             
data LispError = NumArgs        Integer [LispVal]
               | TypeMismatch   String LispVal
               | Parser         ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String
           
-- partially applies Either to LispError, creating a constructor
-- that can be used on any data type           
type ThrowsError = Either LispError               
                            
main :: IO ()
main = do
     args   <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
     
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"     
        
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err  -> throwError $ Parser err
     Right val -> return val
     
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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _ )              = return val
eval (List [Atom "quote", val]) = return val 
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm                    
    = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args 
    = maybe (throwError 
            $ NotFunction "Unrecognized primitive function args" func)
            ($ args)
            (lookup func primitives)
                        
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinop (+)),
              ("-",         numericBinop (-)),
              ("*",         numericBinop (*)),
              ("/",         numericBinop div),
              ("mod",       numericBinop mod),
              ("quotient",  numericBinop quot),
              ("remainder", numericBinop rem),
              ("=",         numBoolBinop (==)),
              ("<",         numBoolBinop (<)),
              (">",         numBoolBinop (>)),
              ("/=",        numBoolBinop (/=)),
              (">=",        numBoolBinop (>=)),
              ("<=",        numBoolBinop (<=)),
              ("&&",        boolBoolBinop (&&)),
              ("||",        boolBoolBinop (||)),
              ("string=?",  strBoolBinop (==)),
              ("string<?",  strBoolBinop (<)),
              ("string>?",  strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)) ]
              

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] 
             -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params 
                            >>= return . Number . foldl1 op
                            
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] 
          -> ThrowsError LispVal
boolBinop unpacker op args 
    = if length args /= 2 
      then throwError $ NumArgs 2 args
      else do left  <- unpacker $ args !! 0
              right <- unpacker $ args !! 1
              return $ Bool $ left `op` right      

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
              
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n 
                       in if null parsed 
                          then throwError $ TypeMismatch "number" 
                                          $ String n
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      
    = "Expected " ++ show expected 
    ++ " args; found values " 
    ++ unwordsList found
showError (TypeMismatch expected found) 
    = "Invalid type: expected " 
    ++ expected ++ ", found " 
    ++ show found
showError (Parser parseErr)             = "Parse error at " 
                                        ++ show parseErr
 
instance Show LispError where show = showError

instance Error LispError where
    noMsg  = Default "An error has occurred."
    strMsg = Default

-- creates an Either monad that always has valid (Right) data
trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- retrieve data from an Either monad
-- intention is to call this function ONLY after a call to trapError
-- [Does this break FP concept; requiring a function to depend
--  on execution order??]
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
        