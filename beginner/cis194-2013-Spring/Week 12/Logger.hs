-- source: Real World Haskell Chapter 14 Monads
-- http://book.realworldhaskell.org/read/monads.html
--
-- Re-writing globRegex so it keeps a record of its translations
module Logger
    (
      Logger
    , Log
    , runLogger
    , record
    ) where
    
import Control.Monad
import Text.Regex.Posix ((=~))
    
type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

instance Monad Logger where
    return a = Logger (a, [])
    -- unwraps the value on the left and passes it to
    -- the function on the right which rewraps everything
    m >>= k  = let (a, w) = execLogger m
                   n      = k a
                   (b, x) = execLogger n
               in Logger (b, w ++ x)
               
runLogger :: Logger a -> (a, Log)
-- evaluate an action, returning the result and the annotations
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])
    
globToRegex :: String -> Logger String
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)
    
globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)
   
charClass :: [Char] -> Logger [Char]   
charClass (']':cs)       = (']':) `liftM` globToRegex' cs
charClass (c:cs)         = (c:)   `liftM` charClass cs

charClass_wordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClass_wordy (c:cs) =
    charClass_wordy cs >>= \ds ->
    return (c:ds)    
    
escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"    
    
-- Example usage
simple = return True :: Logger Bool

ex1 = runLogger simple                              -- (True,[])
ex2 = runLogger (record "hi mom!" >> return 3.1337) -- (3.1337,["hi mom!"])

-- how do you actually pattern match??? Totally missing somethin here :(
