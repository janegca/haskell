-- Source: RWH Chapter 08
--  http://book.realworldhaskell.org/read/

module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' ""             = ""
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"
    
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
-- (=~) text regExp  
--  matches text to a regular expression
name `matchesGlob` pat = name =~ globToRegex pat

-- Example usage
ex1 = "foo.c"     =~ globToRegex "f??.c"   :: Bool      -- True
ex2 = "test.c"    =~ globToRegex "t[ea]s*" :: Bool      -- True
ex3 = "taste.txt" =~ globToRegex "t[ea]s*" :: Bool      -- True

    