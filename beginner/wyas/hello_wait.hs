module Main where

import System.Environment

main :: IO()
main =
    do
        putStrLn ("Enter your name: ")
        name <- getLine     -- waits for input
        putStrLn ("Hello, " ++ name)

{-
    C:\Users\...\wyas>hello_wait
    Enter your name:
    Toby
    Hello, Toby
    
-}        