-- Exercises from Chalmers 2014 TDA452 Functional Programming Course
-- Week 4
-- Source:
--   http://www.cse.chalmers.se/edu/year/2014/course/
--        TDA452_Functional_Programming

import Data.List
import Control.Monad  (unless)
import Test.QuickCheck
import Data.Maybe
import System.Directory
import System.FilePath
import System.Random

{-
    Exercise 0A - Basic IO
    
    Write an IO program which will first read a positive integer, say n, 
    and then reads n integers and writes their sum. The program should 
    prompt appropriately for its inputs and explain its output. 
-}
sumNInts :: IO ()
sumNInts = do putStrLn "Enter number of digits to be summed."
              count <- getInt
              putStrLn "Enter your numbers."
              sumNums count 0 
    where
        sumNums :: Int -> Int -> IO ()
        sumNums c sum | c <= 0 = putStrLn $ "The total is: " ++ show sum
                      | otherwise = do n <- getInt
                                       sumNums (c - 1) (sum + n)
                                              
getInt :: IO Int
getInt = do line <- getLine
            return (read line :: Int)        
                                       
{-
    Exercise 0B - Basic IO
    
    Write a program which repeatedly reads integers (one per line) until 
    finding a zero value and outputs a sorted version of the inputs read. 
    Which sorting algorithm is most appropriate in such a case? 
-}                                       
sortInts :: IO ()
sortInts = do putStrLn "Enter some numbers. Zero to quit."
              getNums []
    where
        getNums :: [Int] -> IO ()
        getNums nums = do  n <- getInt
                           if n == 0
                           then putStrLn $ show (sort nums)
                           else getNums (n : nums)
                  
{-
    Exercise 0C - Basic IO
    
    Define the function

        repeat :: IO Bool -> IO () -> IO ()

    such that repeat test op has the effect of repeating op until the 
    condition test is True. 
    
    Source for solution:
    https://github.com/gregorulm/tda452_exercises/blob/master/ex4.hs
    
    Examples:
        repeat' (fmap (== "stop") getLine) (putStrLn "foo")
        
-}            
repeat' :: IO Bool -> IO () -> IO ()
repeat' test op = do res <- test
                     unless res (do op
                                    repeat' test op)
                                    
{-
    Exercise 1
    
    Consider the following standard Haskell function, that looks up an 
    element in a list of pairs (table):

        look :: Eq a => a -> [(a,b)] -> Maybe b
        look x []           = Nothing
        look x ((x',y):xys)
          | x == x'         = Just y
          | otherwise       = look x xys

    Define a property prop_LookNothing that expresses that if the look
    function delivers Nothing, then the thing we were looking for was not 
    in the table.

    Also define a property prop_LookJust that expresses that if the look
    function delivers a result (Just y), then the pair (x,y) should have 
    been in the table.

    Also write one property prop_Look that combines prop_LookNothing and 
    prop_Just into one property.     

-}                
look :: Eq a => a -> [(a,b)] -> Maybe b
look x []           = Nothing
look x ((x',y):xys)
  | x == x'         = Just y
  | otherwise       = look x xys

prop_LookNothing :: (Eq a, Eq b) => a -> [(a,b)] -> Bool  
prop_LookNothing k xs = (look k xs == Nothing)
                     == (not (elem k (map fst xs)))
   
prop_LookJust :: (Eq a, Eq b) => a -> [(a,b)] -> Property   
prop_LookJust k xs =  
    not (null xs) ==> look k xs == 
                      let res = [ Just y | (x,y) <- xs, x == k]
                      in  if null res then Nothing else head res

-- couldn't get a generalized version of this to work 
-- quickCheck generates the simplest type it knows, which in this
-- case is [((),()),((),())]                     
prop_Look :: Int -> [(Int,Int)] -> Bool
prop_Look k xs = isValid (look k xs)
    where
        isValid Nothing  = null res
        isValid (Just y) = [y] == res
        
        res = [y | (x,y) <- xs, k == x]
                
            
{-
    Exercise 3 The Number Guessing Game
    
    In this exercise, you are going to implement the "number guessing 
    game" in Haskell.

    Here is an example of how this might work:

      Main> game
      Think of a number between 1 and 100!
      Is it 50? higher
      Is it 75? lower
      Is it 62? lower
      Is it 56? yes
      Great, I won!

    The text in italics is what the user types in. The other text is 
    produced by your program.

    Implement a function

      game :: IO ()

    That plays one instance of this game.

    You might need the following functions:

      getLine :: IO String         -- reads a line of user input
      putStrLn :: String -> IO ()  -- outputs one line of text

    Before you start programming, think of a good guessing strategy for 
    the computer that minimizes the number of guesses!     
    
-}
game :: IO ()
game = do putStrLn "Think of a number between 1 and 100"
          putGuess 50 100
    where
        putGuess n prev = do putStr $ "Is it " ++ show n ++ "? "
                             resp <- getLine
                             nextGuess resp n prev
                             return ()
                             
        nextGuess r n prev
            | r == "higher" = putGuess (n + abs(n - prev) `div` 2) n
            | r == "lower"  = putGuess (n - abs(n - prev) `div` 2) n
            | r == "yes"    = putStrLn "Great, I won!"
            | otherwise     = error "Enter higher, lower or yes"

-- alternative method from
-- https://github.com/gregorulm/tda452_exercises/blob/master/ex4.hs
game' :: IO ()
game' = do putStrLn "Think of a number between 1 and 100!"
           loop 0 100
           where loop low high = do
                   putStrLn $ "Is it " ++ show avg ++ "?"
                   val <- getLine
                   case val of
                      "yes" -> putStrLn "Great, I won!"
                      "higher" -> loop avg high
                      "lower"  -> loop low avg
                   where avg = (low + high) `div` 2             

{-
    Exercise 4. A Backup Script

    Haskell programs can generate instructions to be performed by the 
    command shell using

        system :: String -> IO ()

    This is one way to run other programs from Haskell, for example. 
    The String passed to system often differs, depending on whether it 
    is the Linux shell or the Windows one which should obey the command. 
    
    For example, to copy file A to file B under Linux, the string "cp A B" 
    is used, while under Windows it would be "copy A B". As a result, 
    programs which use system normally work only under one operating 
    system, which is sad, but can't be helped.

    Use system to write a program that

        creates a new directory called "backup", using the "mkdir" 
        command,
        
        copies all the files in the current directory into the backup, 
        using cp or copy as appropriate

    (You are supposed to copy the files one by one, not by constructing 
    a command such as "cp *  backup", because filename expansion isn't 
    applied to strings passed to system.)

    You will need to read the list of filenames in the current directory, 
    and to find out how, you will need to consult the documentation of
    Haskell's standard libraries. Here's the link:

    http://www.haskell.org/ghc/docs/latest/html/libraries/index.html

    There are a lot of them! The one you want is called System.Directory--
    find its documentation, and figure out how to read the filenames in a
    directory.

    You will also perhaps need to perform all of a list of actions. You 
    may find the function

        sequence :: Monad m => [m a] -> m [a]

    useful for this. You must add

        import Monad

    to your file if you want to use it.

    If you want to, you can also use the standard functions 
    createDirectory and copyFile from System.Directory (instead of 
    the commands "mkdir" and "cp") to accomplish this.     

-}                   
backup :: IO ()
backup = do -- setup directories
            cfp <- getCurrentDirectory
            let nfp = joinPath [cfp, "backup"]  
            createDirectoryIfMissing False nfp
            
            -- get Haskell file names
            files <- getDirectoryContents cfp
            let hfiles = [ f | f <- files, takeExtension f == ".hs"]
                        
            -- backup the Haskell files
            _ <- sequence [ copyFile f (joinPath [nfp,f]) | f <- hfiles]            
            putStrLn "Finished backup"

{-
    5 (*). Generating Lists

    Sometimes we want to generate lists of a certain length.

    A. Write a generator

        listOf :: Integer -> Gen a -> Gen [a]

    such that listOf n g generates a list of n elements, where each 
    element is generated by g. What property would you write to test 
    that your generator behaves as it should?

    B. Now use listOf to write a generator that generates pairs of lists 
       of the same random length.

    C.Take a look at the standard Haskell functions zip and unzip:

      zip :: [a] -> [b] -> [(a,b)]
      unzip :: [(a,b)] -> ([a],[b])

    Write down two properties for these; one that says that zip is the 
    inverse of unzip, and one that unzip is the inverse of zip.
    Note that unzip is not always the inverse of zip, so you need a 
    condition! Could you make use of the generator you just defined?  

    NOTES:
        Gen a is defined in Test.QuickCheck
        
        To see examples of the Gen a outputs, use 'sample'

-}            
r1to100 :: Gen Integer
r1to100 = do r <- choose (1,100)
             return r
             
exR1to100 = sample r1to100             
         
listOf' :: Integer -> Gen a -> Gen [a]
listOf' n g = sequence [ g | _ <- [1..n]]

lstR = listOf' 4 r1to100

showListR xs =  mapM_ print xs



