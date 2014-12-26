-- Informatics 1 - Functional Programming 
-- Lecture 15-16 - IO and Monads
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect15.pdf
-- Video: 17/11/2014
--        20/11/2012
--
-- Related files:
--     Confused.hs, Echo.hs, Exp.hs, Parser.hs

import Data.Char (toUpper)
import Control.Monad (guard)

{-
    All functions in Haskell are pure in that they have no side-effects;
    a function, given the same input will always produce the same output.
    
    Side-effects make parallel computation very difficult so having
    time independent (pure) functions is very useful.
    
    In Haskell, everything is done with types; there is a type, IO (),
    that allows Haskell code to produce side-effects (output to screen,
    file, etc).

    Ex: 
        putChar :: Char -> IO ()        -- Prelude function
    
        which is read as a function, putChar, that takes a character
        and produces a command: IO ()
        
        the output occurs only when the command is performed
        i.e. *Main> putChar '!'    -> !
        
    You can combine two commands
    
    Ex
        (>>) :: IO () -> IO () -> IO ()     -- Prelude operator
        
        this will join the results of two commands
        
        ie. *Main> putChar '?' >> putChar '!'      -> ?!
        
    The 'identity' for the IO () command is:
        done :: IO ()           -- NOT in Prelude
        
    which does -- Nothing
    
    It is useful for indicating that nothing more is to be done
    
    Example (from Prelude)
        putStr :: String -> IO ()
        putStr []     = done
        putStr (x:xs) = putChar x >> putStr xs
        
        which, for putStr "?!",  is the same as:
            putChar '?' >> (putChar '!' >> done)
            
    The function can also be written using higher order functions
    
        putStr :: String -> IO ()
        putStr = foldr (>>) done . map putChar    
        
    The operator (>>) has identity 'done' and is associative
    
        m >> done = m
        done >> m = m
        (m >> n) >> o = m >> (n >> o)    
-}
{-
    MAIN
    
        declare a 'main' function:
            main :: IO ()
            
        entry point for executing a compiled program 
        'runghc' compiles a .hs file and runs 'main'
        
        Examples:
            from the command prompt, 
                runghc Confused.hs  -> outputs ?!
                
    You can combine an arbitrary number of commands/functions
    to produce a full application.
-}
{-
    EQUATIONAL REASONING
    
        Substituting equals for equals always holds
        
        eg
        Prelude> let m = putStrLn "ha" in m >> m
        ha
        ha

        output is two ha's, i.e. 'm' is = putStrLn "ha" for all m's
        
-}
{-
    COMMAND VALUES
    
    The same things works with input.
    
    The commands themselves have no value; the value you get from 
    printing something is not of interest ... it is the printing itself
    is what's important; so type: IO () is an indication that we
    should expect a side-effect (like the printing of a string) BUT
    not a value.
    
    If we read something, the value important.
    i.e.    getChar :: IO Char
    
            here we are also performing an IO operation but now
            we expect to get back a value, in particular, a Char
            value.
            
            Prelude> getChar
            a
            'a'

            getChar waits for a character from the keyboard and
            then echoes it back
            
    Now we need a command that will return the value
        return ::  a -> IO a       --> yields the input when executed
        
        return [] :: IO String     --> yields an empty string
        
    (see echo.hs for an example of get input and returning it
-}
{-
    COMBINING COMMANDS WITH VALUES
    
    We combine commands with an operator written >>= and 
    pronounced “bind”.
        (>>=) :: IO a -> (a -> IO b) -> IO b
    
    For example, performing the command
        getChar >>= \x -> putChar (toUpper x)
    
    when the input is "abc" produces the output "A", and the remaining 
    input is "bc".    

    The “bind” operator in detail
        (>>=) :: IO a -> (a -> IO b) -> IO b
    If
        m :: IO a
    is a command yielding a value of type a, and
        k :: a -> IO b
    is a function from a value of type a to a command yielding a value of type b, then
        m >>= k :: IO b
    is the command that, if it is ever performed, behaves as follows:
         first perform command m yielding a value x of type a;
    then perform command k x yielding a value y of type b;
    then yield the final value y.    
    
-}         
{-
    USING DO NOTATION
    
    The Do notation is syntactic sugar as the 'bind' lambda notation
    is a little clumsy.
    
    The two getLine functions below are equivalent, as are
    the two readLine functions.
    
    NOTE: the lines that call 'commands' must end with semi-colons.

-}   
getLine' :: IO String
getLine' = getChar >>= \x ->
    if x == '\n' then
        return []
    else
        getLine' >>= \xs ->
        return (x:xs)
        
getLine'' :: IO String
getLine'' = do {                -- use curly brackets
    x <- getChar;               -- commands have semi-colons between them
    if x == '\n' then
        return []
    else do {
        xs <- getLine;
        return (x:xs)
        }
    }        
    
echo :: IO ()
echo = getLine >>= \line ->
    if line == "" then
        return ()
    else
        putStrLn (map toUpper line) >>
        echo    
        
echo' :: IO ()
echo' = do {
    line <- getLine;        -- commands need semi-colons to seaprate them
    if line == "" then
        return ()
    else do {
        putStrLn (map toUpper line);  -- command, needs semi-colon
        echo'
        }
    }
        
{-
    “Do” notation in general
    Each line x <- e; ... becomes e >>= \x -> ...
    Each line e; ... becomes e >> ...

    For example,
        do { x1 <- e1;
            x2 <- e2;
            e3;
            x4 <- e4;
            e5;
            e6 }
    
    is equivalent to
    e1 >>= \x1 ->
    e2 >>= \x2 ->
    e3 >>
    e4 >>= \x4 ->
    e5 >>
    e6

-}
{-
    MONOIDS

    A monoid is a pair of an operator (@@) and a value u, where the 
    operator has the value as identity and is associative.
    
    Basically a mathematical structure consisting of an operator and
    and identity.

        u @@ x = x
        x @@ u = x
        (x @@ y) @@ z = x @@ (y @@ z)

    Examples of monoids:
        (+) and 0               -- 0 is the identity for addition
        (*) and 1               -- 1 is the identity for multiplication
        (||) and False          -- False is the identiry for logical OR
        (&&) and True           -- True is the identity for logical AND
        (++) and []             -- an empty list is identity for append
        (>>) and done           -- done is identity for sequencing oper
                                -- NOTE: (>>) read as 'then'
-}
{-

    MONADS
    A 'monad' is a generalization of monoids.
    
    We know that (>>) and done satisfy the laws of a monoid.
    
        done >> m = m
        m >> done = m
        (m >> n) >> o = m >> (n >> o)

    Similarly, (>>=) and return satisfy the laws of a monad.

        return v >>= \x -> m       = m[x:=v]
        m >>= \x -> return x       = m
        (m >>= \x -> n) >>= \y-> o = m >>= \x -> (n >>= \y -> o)
        
    'return' is basically a kind of 'identity' for the bind operation
    they are not exactly identities and associative laws
    
    A little easier to understand in light of 'let' notation where
    the three monad laws have analogues in "let" notation
    
        let x = v in m = m[x:=v]
        let x = m in x = m
        let y = (let x = m in n) in o
                         = let x = m in (let y = n in o)    
                    
    In the first law, you can replace 'v' by any term [that does not
    have a side-effect??] and not just any value.
-}        
{-
    ROLL YOUR OWN MONAD
    
    Monad is a type class
    
        class Monad m where
            return :: a -> m a
            (>>=) :: m a -> (a -> m b) -> m b

        where 'm' is a 'type constructor'
        and we need a return operation and a bind operation
        
    there is also a MonadPlus class
    
        class MonadPlus m where
            mzero :: m a
            mplus :: m a -> m a -> m a    
            
        where 'm' is a 'type constructor'
        and we need to define 'mzero' and 'mplus'
-}
{-
    The monad of lists
    
    Lists form monads.
    
    The two definitions of pair, below, are equivalent.
    
-}
pairs :: Int -> [(Int, Int)]
pairs n = [ (i,j) | i <- [1..n], j <- [(i+1)..n] ]

pairs' :: Int -> [(Int, Int)]
pairs' n = do {
    i <- [1..n];
    j <- [(i+1)..n];
    return (i,j)
    }
    
{-
    Example output:
    
        *Main> pairs 4
        [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
        *Main> pairs' 4
        [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
    
    Could also have writtent he above with 'guards'
-}   
pairs'' :: Int -> [(Int, Int)]
pairs'' n = [ (i,j) | i <- [1..n], j <- [1..n], i < j ]

pairs''' :: Int -> [(Int, Int)]
pairs''' n = do {
        i <- [1..n];
        j <- [1..n];
        guard (i < j);  -- requires import of Control.Monad
        return (i,j)
    }
     
{-

    Example output:
    
        *Main> pairs'' 4
        [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
        *Main> pairs''' 4
        [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
        *Main>     
        
    Comprehension notation is a by-product out of monads
-}
{-
    PARSERS
    
    Parser will be a monad of type
        type Parser a = String -> [(a, String)]
        
    We're going to build bigger parsers from little parser
    A Parser is an abstract data type
    
        data Parser a = Parser (String -> [(a, String)])
        
    we'll have a method to 'apply' a Parser and a function, parse,
    that uses 'apply'
    
        apply (Parser f) s = f s
        parse m s = head [x | (x,t) <- apply m s, t = "" ]
        
    you then have to create an instance of the Parser as a Monad
    
        instance Monad Parser where
            return x = Parser (\s -> [(x,s)])
            m >>= k  = Parser (\s ->
                            [ (y, u) |
                              (x, t) <- apply m s,
                              (y, u) <- apply (k x) t ])    
        
        t is what remains in the string once you've parsed 'x'
        here we are parsing one thing followed by another thing
        i.e. once you parsed 'x', depending on what you found
             in that parse, you want to do something else, k, 
             with that same x
             
    we also want an instance of Parser that handles 'alternatives'
    versus doing one thing after another
    
        instance MonadPlus Parser where
            mzero     = Parser (\s -> [])
            mplus m n = Parser (\s -> apply m s ++ apply n s)    
    
    we created parsers for
        - single characters
        - a character satisfying a predicate (i.e. isDigit, isOdd, etc)
        - a given character
        - a string
        - a sequence of zero or more and of one or more
        - match a natural number
        - match a negative number
        - match an integer
        
    the example parser in 'parser.hs' is used to parse expressions
    of the type Exp defined in 'exp.hs'
    
   
-}

        