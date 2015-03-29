module TestMain where
{-
    Week 08 - IO - Notes and Exercises
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
    
    Example of compiling a program with a module name.
    
    Converts user input to upper case or use a pipe to convert
    a file to upper case
    
    compile:
        ghc -main-is TestMain -o MyMain testMain.hs
        
        where
            -main-is    gives the module to be used as Main
            -o          name to be used for .exe file
   
-}
import Data.Char (toUpper)

main = interact ((++) "Your data, in uppercase, is:\n\n" . 
                 map toUpper)       
