module Main where

import System.Environment

main :: IO()
main =
    do
        args <- getArgs
        putStrLn (show (read (args !! 0) +
                        read (args !! 1)))

{-
    C:\Users\...\wyas>hello_add 5 6
    11
-}                            