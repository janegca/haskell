{-
    Ref: Real World Haskell - Chapter 7 I/O
         http://book.realworldhaskell.org/read/io.html
         
    Converts user input to upper case or use a pipe to convert
    a file to upper case
    
    compile: ghc exInteract01
    
    usage:  exInteract01 < input.txt        -- to convert file
            exInteract01                    -- to convert user input
-}
import Data.Char (toUpper)

main = interact ((++) "Your data, in uppercase, is:\n\n" . 
                 map toUpper)       
