module Main where

import System.Environment

main :: IO()
main =
    do
        args <- getArgs
        putStrLn ("Hello, " ++ (args !! 0))
        
{--
    Compile using:  ghc hello.hs
        
    Example usage:
    
    C:\Users\...\beginner\wyas>hello Jane
    Hello, Jane
--}        
    