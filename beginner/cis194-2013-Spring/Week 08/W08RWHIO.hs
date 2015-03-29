module W08RWHIO where

{-
    Week 08 - IO
        Notes from recommended readings
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html


    Ref: Real World Haskell - Chapter 7 I/O
         http://book.realworldhaskell.org/read/io.html
-}
{-
    I/O Actions
    -----------
        IO actions alert us to the fact the function will produce
        a 'side effect' (alter the state of the 'world' in some manner)
        
        have the type IO t
        are first-class values 
        produce an effect only when performed (executed in an IO context)
        
        the 'do' construct allows us to sequence actions
        the value of a 'do' block is the value of last excuted action
-}

import System.IO
import Data.Char (toUpper)

-- example of using a pure function in an IO action
-- name2reply ALWAYS returns the same result for the same input
name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

-- exCallPure will return different results according to
-- user input    
exCallPure :: IO ()
exCallPure = do
       putStrLn "Greetings once again.  What is your name?"
       inpStr <- getLine
       let outStr = name2reply inpStr
       putStrLn outStr
       
-- example of reading and writing files
exRWFile :: IO ()
exRWFile = do 
       -- get handles to required files
       inh  <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       
       -- do what we need to do to the files
       mainloop inh outh
       
       -- make sure we close and release the files before exiting
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh      -- check for end of file
       if ineof
           then return ()           
           else do -- get a line from the file
                   inpStr <- hGetLine inh   
           
                   -- a PutStrLn method that takes a file handle
                   -- writes a line to a file
                   hPutStrLn outh (map toUpper inpStr)
                   
                   -- read the rest of the file
                   mainloop inh outh   
                   
-- do the same as above but do it lazily
-- i.e. read the file as needed vs all at once
exLazyRWFile :: IO ()
exLazyRWFile = do 
       inh    <- openFile "input.txt" ReadMode
       outh   <- openFile "output.txt" WriteMode
       
       -- this one function, hGetContents, handles reading the
       -- entire file for us; no need for loops, checks on eof, etc.
       inpStr <- hGetContents inh
       
       -- use a pure function, processData, to convert the file contents
       let result = processData inpStr
       hPutStr outh result
       
       hClose inh
       hClose outh

processData :: String -> String
processData = map toUpper                   

-- actually can do all of the above with just 3 lines of code
-- read/writeFile work lazily; you don't need the file handles
-- as you don't need to close the files; the file housekeeping
-- is handled internally
exShortRWFile = do 
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)
       
-- you can use the 'interact' function to get the filenames or other
-- input from the user; see exInteract01 and exInteract02 which must
-- be compiled

{-
    IO Monad
    --------
    
        <-         operator pulls values out of monads
        return      wraps values in a monad
    
-}
-- given a string, return an action that can print the string
str2action :: String -> IO ()
str2action input = putStrLn ("Data: " ++ input)

-- given a list of strings, convert them to actions 
-- key point is that the 'actions' do not get evaluated on their creation
-- but only when they are executed 
list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2actions strings

printitall :: IO ()
printitall = runall actions

-- Take a list of actions, and execute each of them in turn.
runall :: [IO ()] -> IO ()
runall [] = return ()
runall (firstelem : remainingelems) = 
    do firstelem
       runall remainingelems

exActions = do str2action "Start of the program"
               printitall
               str2action "Done!"
                 
{-

    *W08RWHIO> exActions
    Data: Start of the program
    Data: 1
    Data: 2
    Data: 3
    Data: 4
    Data: 5
    Data: 6
    Data: 7
    Data: 8
    Data: 9
    Data: 10
    Data: Done!
    *W08RWHIO> 
-}       

-- the above can be written more concisely as
str2message' :: String -> String
str2message' input = "Data: " ++ input

str2action' :: String -> IO ()
str2action' = putStrLn . str2message'

numbers' :: [Int]
numbers' = [1..10]

exActions2 = do str2action' "Start of the program"

                -- mapM_ sequentially maps, and executes,
                -- an action to a list
                mapM_ (str2action' . show) numbers'
                
                str2action' "Done!"
                
-- a 'do-block' is actually syntactic sugar for the sequence operators
-- (>>), (>>=), we could write a basic io function as
exBasicIO =
       putStrLn "Greetings!  What is your name?" 
    >> getLine
    >>= (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")  

-- the 'do' construct just makes things a little easier to read
exBasicIO2 = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"    
       
{-

    Return
    ------
        the return function is used to wrap data in a Monad
        necessary as all functions using monads must return an IO
        value
        
        can be used anywhere within a do block without ending
        function execution
-}       
-- wrapping a pure function as an IO value
isYes :: String -> Bool
isYes inpStr = (toUpper . head $ inpStr) == 'Y'

isGreen :: IO Bool
isGreen =
    do putStrLn "Is green your favourite color?"
       inpStr <- getLine
       return (isYes inpStr)       
       
returnTest :: IO ()
returnTest =
    do -- return wraps '1' in a monad, <- unwraps it
       one <- return 1      -- a return DOES NOT END EXECUTION
       let two = 2
       putStrLn $ show (one + two)

{-
    Buffering
    ---------
        There are 3 different BufferMode's in Haskell:
            NoBuffering
            LineBuffering
            BlockBuffering  -- unusable in interactive mode
                            -- takes a Maybe param for block size
                            -- default (if Nothing) is specific to the
                            -- OS and Haskell compiler implementation
                            
        You can both get and set the buffering.
        
        The buffer is flushed when hClose is called or you when you
        use fFlush.
                            
-}
currBuffering = hGetBuffering stdin     -- NoBuffering
setBuffering  = hSetBuffering stdin (BlockBuffering (Just 4096))
newBuffering  = hGetBuffering stdin
setBuffering1  = hSetBuffering stdin NoBuffering
newBuffering1 = hGetBuffering stdin

{-

    *W08RWHIO> currBuffering
    NoBuffering
    *W08RWHIO> setBuffering
    *W08RWHIO> newBuffering
    BlockBuffering (Just 4096)
    *W08RWHIO> setBuffering1
    *W08RWHIO> newBuffering1
    NoBuffering
    *W08RWHIO> 

-}
{-
    The Environment and Command Line Arguments
    ------------------------------------------
        Variety of functions for interacting with execution environment:
        
            System.Environment.getArgs
            System.Environment.getProgName
            System.Console.GetOpt           -- to parse cmd line options
            
            getEnvironment         -- returns the entire environment
                                   -- use lookup to find one or more vars
            getEnvironment.getEnv  -- returns a specific variable
            
        [NOTE: RWH says there is no cross product way to set and unset
               environment variables; this is no longer true, 
               you can set and unset environment variables using:
                    setEnv
                    unsetEnv
        ]
            

-}

  