{-# LANGUAGE ExistentialQuantification #-}

module Main where
{-
    Chapter 5
        Implement conditional and basic list functions
        
        Pragma Added:
            ExistentialQuantification   -- allow heterogeneous lists
            
        Data type added:
            Unpacker        -- hold any (LispVal -> something) function
            
        Functions modified:
            eval            -- added clause for 'if-then' conditionals
            primitives      -- add new list functions
            
        Functions added:
            car             -- in Scheme, returns the head of a list
            cdr             -- in Scheme, returns the tail of a list
            cons            -- in Scheme, (as in Hakell), list constructor
            unpackEquals
            
    Reference for Scheme functions
        http://www.dotnetperls.com/cons-car-cdr-cadr-scheme
        
    Example behaviours in Scheme:
        
        (car '(a b c)) = a
        (car '(a)) = a
        (car '(a b . c)) = a
        (car 'a) = error – not a list
        (car 'a 'b) = error – car only takes one argument
        
        (cdr '(a b c)) = (b c)
        (cdr '(a b)) = (b)
        (cdr '(a)) = NIL
        (cdr '(a . b)) = b
        (cdr '(a b . c)) = (b . c)
        (cdr 'a) = error – not a list
        (cdr 'a 'b) = error – too many arguments      
        
    Examples
    
        *Main> :main "(cdr '(a simple test))"
        (simple test)
        *Main> :main "(car (cdr '(a simple test)))"
        simple
        *Main> :main "(car '((this is) a test))"
        (this is)
        *Main> :main "(cons '(this is) 'test)"
        ((this is).test)
        *Main> :main "(cons '(this is) '())"
        ((this is))
        *Main> :main "(eqv? 1 3)"
        #f
        *Main> :main "(eqv? 3 3)"
        #t
        *Main> :main "(eqv? 'atom 'atom)"
        #t
        
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
               
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)               
                    
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
eval (List [Atom "if", pred, conseq, alt]) =        -- added clause
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
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
              ("string>=?", strBoolBinop (>=)),
              ("car",       car),
              ("cdr",       cdr),
              ("cons",      cons),
              ("eq?",       eqv),
              ("eqv?",      eqv),
              ("equal?",    equal)]
              

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

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- mimic Scheme function to return the head of a list
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

-- mimic Scheme function to return the tail of a list
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList        

-- mimic Scheme list constructor
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

-- handle Scheme equivalence fns: eq? eqv? equal?
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], 
                                                  List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             
    = return $ Bool $ (length arg1 == length arg2) 
                       && (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) 
            = case eqv [x1, x2] of
                Left err -> False
                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError 
                                           $ NumArgs 2 badArgList
     
-- check if two LispVals are equal     
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, 
                          AnyUnpacker unpackStr, 
                          AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList        
        
