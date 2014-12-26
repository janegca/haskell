-- Informatics 1 - Functional Programming 
-- Lecture 15-16 - IO and Monads

module Parser(Parser,apply,parse,char,spot,
  token,star,plus,parseInt) where

import Data.Char
import Control.Monad

-- The type of parsers
newtype Parser a = Parser (String -> [(a, String)])

-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s  =  f s

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
parse m s  =  one [ x | (x,t) <- apply m s, t == "" ]
  where
  one []                  =  error "no parse"
  one [x]                 =  x
  one xs | length xs > 1  =  error "ambiguous parse"

-- Parsers form a monad

--   class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b

instance Monad Parser where
  return x  =  Parser (\s -> [(x,s)])
  m >>= k   =  Parser (\s ->
                 [ (y, u) |
                   (x, t) <- apply m s,
                   (y, u) <- apply (k x) t ])

-- Parsers form a monad with sums

--   class MonadPlus m where
--     mzero :: m a
--     mplus :: m a -> m a -> m a

instance MonadPlus Parser where
  mzero      =  Parser (\s -> [])
  mplus m n  =  Parser (\s -> apply m s ++ apply n s)

-- Parse one character
char :: Parser Char
char =  Parser f
  where
  f []     =  []
  f (c:s)  =  [(c,s)]

-- guard :: MonadPlus m => Bool -> m ()
-- guard False  =  mzero
-- guard True   =  return ()

-- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p  =  do { c <- char; guard (p c); return c }

-- Match a given character
token :: Char -> Parser Char
token c  =  spot (== c)

-- Perform a list of commands, returning a list of values
-- sequence :: Monad m => [m a] -> m [a]
-- sequence []
-- sequence (m:ms)  =  do {
--                       x <- m;
--                       xs <- sequence ms;
--                       return (x:xs)
--                     }

-- match a given string (defined two ways)
match :: String -> Parser String
match []      =  return []
match (x:xs)  =  do {
                   y <- token x;
                   ys <- match xs;
                   return (y:ys)
                 }

match' :: String -> Parser String
match' xs  =  sequence (map token xs)

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p  =  plus p `mplus` return []

-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p  =  do x <- p
              xs <- star p
              return (x:xs)

-- match a natural number
parseNat :: Parser Int
parseNat =  do s <- plus (spot isDigit)
               return (read s)

-- match a negative number
parseNeg :: Parser Int
parseNeg =  do token '-'
               n <- parseNat
               return (-n)

-- match an integer
parseInt :: Parser Int
parseInt =  parseNat `mplus` parseNeg


