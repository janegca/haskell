-- Informatics 1 - Functional Programming 
-- Lecture 15-16 - IO and Monads

module Echo where

import Data.Char

echo :: IO ()
echo =  do {
          line <- getLine;
          if line == "" then
            return ()
          else do {
            putStrLn (map toUpper line);
            echo
          }
        }

main :: IO ()
main =  echo
