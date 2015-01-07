module Main where

import System.Environment

main :: IO()
main =
    do
        args <- getArgs
        putStrLn ("Hello, " ++ (args !! 0) 
                            ++ ' ' : (args !! 1))

{-
    C:\Users\...\wyas>hello_1 John Lennon
    Hello, John Lennon
-}                            