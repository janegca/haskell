module W08RIO where

{-
    Week 08 - IO
        Notes from recommended readings
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-} 
{-
    Ref: Learn You a Haskell Chapter 9: Input and Output
         http://learnyouahaskell.com/input-and-output
-}


import Control.Monad
import Data.Char (toUpper)
import System.IO
import System.Directory
import Data.List
import System.Environment
import System.Random

-- do/until loop
main = do   
    putStrLn "Enter some text, empty line to exit."
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 

-- return is just another function
main1 = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b  
    
-- can use 'let' in place of (<-)
main2 = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b      
    

-- get characters until an empty space is encountered  
-- due to buffering, execution only starts when we hit return
-- try typeing: hello sir, only 'hello' will be printed  
main3 = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main3  
        else return ()

-- does the same as main3 only uses a 'when'
main4 = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main4
        
-- using sequence to process a list of I/O actions
main5 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
 
-- mapping an IO function produces a list of IO actions
-- that can be executed using sequence 
-- (Note: the final value is a list of all the returned tuples
--        as 'getLine' returns IO String)
seq1 = sequence (map print [1,2,3])

-- use mapM to do the same
seq2 = mapM print [1,2,3]       -- retains the final result
seq3 = mapM_ print [1,2,3]      -- discard the final result

{-

    *W08RIO> seq1
    1
    2
    3
    [(),(),()]
    *W08RIO> seq2
    1
    2
    3
    [(),(),()]
    *W08RIO> seq3
    1
    2
    3
    *W08RIO> 

-}

-- forM is like mapM but it reverses the args, taking the list
-- first and function to be mapped over it, second
main6 = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    --mapM_ putStrLn colors  
    forM_ colors putStrLn
    
-- read a file
main7 = do  
    contents <- readFile "shortlines.txt"  
    putStr contents 
    
-- write to a file
main8 = do     
    contents <- readFile "shortlines.txt"     
    writeFile "shortlinescap.txt" (map toUpper contents) 

-- append to a file
main9 = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")    
    
-- using a file as a stream
main10 = do   
    withFile "shortlinescap.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents)   

-- a small program
main11 = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"     

-- getting command line args (need System.Environment)
-- (need to compile it and run from command line to see it work)
main12 = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName     
   
{-
    Randomness
    ----------
    Use System.Random package
        RandomGen - type class for types that can act as a source of
                    randomness
                    StdGen - an instance of RandomGen
                    
        Random    - type class for things that can take random values

    mkStdGen seed
        returns a tuple containing a random number and a new
        random number generator
        if called with the same seed, returns the same number
        i.e. not a new random number
-}
r1 = random (mkStdGen 100) :: (Int, StdGen)     
r2 = random (mkStdGen 100) :: (Int, StdGen)
tr12 = fst r1 == fst r2

-- need to change seed
r3 = random (mkStdGen 949494) ::(Int, StdGen)

-- can use different types to return bools, floats, etc
r4 = random (mkStdGen 943282) ::(Bool, StdGen)
r5 = random (mkStdGen 34021) :: (Float, StdGen)

-- simulate tossing three coins
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen)   = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  
    
tc1 = threeCoins (mkStdGen 21)      -- (True,True,True)
tc2 = threeCoins (mkStdGen 95)      -- (True,False,False)
tc3 = threeCoins (mkStdGen 102)     -- (True,True,True)

-- the 'randoms' function will return an infinite sequence
rs1 = take 5 $ randoms (mkStdGen 11) :: [Int]
rs2 = take 5 $ randoms (mkStdGen 110) :: [Bool]

-- randomR creates random numbers within a specified range
rr1 = randomR (1,6) (mkStdGen 349393) :: (Int, StdGen)
rr2 = randomR (1,6) (mkStdGen 23412) :: (Int, StdGen)

-- randomRs creates a stream of random values in the specified range
-- Note: produces the same stream given the same seed regardless of
--       time or system being used (the result of rrs1 is EXACTLY
--       the same as the result in the text
rrs1 = take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

-- a better way to generate random values using 'getStdGen'
-- which will create a global generator [this only works if
-- compiled and run from the command line, in ghci always
-- produces the same string]
main13 = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen) 
    
-- using an infinite stream to get 20 chars 
-- (this produces different strings even in ghci)
main14 = do  
    gen <- getStdGen  
    let randomChars = randomRs ('a','z') gen  
        (first20, rest) = splitAt 20 randomChars  
        (second20, _)   = splitAt 20 rest  
    putStrLn first20  
    putStr second20     
    
-- another option is to use newStdGen to update the global generator
-- (again, this produces different strings even in ghci)    
main15 = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen')    
    
{-
    Bytestrings
    -----------
    
    A better way to read file streams. Two varities
        Data.Bytestring         -- strict functions
        Data.Bytestring.Lazy    -- lazy functions
        
    Both have a number of functions that allow you to 'chunk' file
    reads.
        
-}       
{-
    Exceptions
    ----------
    System.IO.Error provides a function:

        catch :: IO a -> (IOError -> IO a) -> IO a
        
    which takes two functions: an IO action and an error handler for 
    the action
 
    [Note: name changed to 'catchIOError' ...see exCatch.hs]
    
    System.IO.Error has a number of error handling functions:
        isAlreadyExistsError
        isDoesNoteExistError
        isAlreadyInUseError
        isFullError
        isEOFError
        isIllegalOperation
        isPersmissionError
        isUserError
        
    along with a number of others
-} 