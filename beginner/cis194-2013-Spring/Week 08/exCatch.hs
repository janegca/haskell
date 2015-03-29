{-
    Ref: Learn You a Haskell Chapter 9: Input and Output
         http://learnyouahaskell.com/input-and-output
         
    Using 'catch' to trap and handle IO errors
    
    compile:  gch -o exCatch
    
    usage: exCatch "company.txt"
           exCatch "nosuchfile.txt"
-}
import System.Environment
import System.IO
import System.IO.Error

main = catchIOError toTry  handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise             = ioError e   -- re-throw unexpected errors
    
