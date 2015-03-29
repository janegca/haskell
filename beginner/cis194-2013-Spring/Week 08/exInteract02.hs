{-
    Ref: Real World Haskell - Chapter 7 I/O
         http://book.realworldhaskell.org/read/io.html
         
    Only display lines that contain the letter a
    
    compile: ghc exInteract02
    
    usage:  exInteract01 < input.txt        -- to convert file
            exInteract01                    -- to convert user input
-}
import Data.Char (toUpper)

main = interact (unlines . filter (elem 'a') . lines)