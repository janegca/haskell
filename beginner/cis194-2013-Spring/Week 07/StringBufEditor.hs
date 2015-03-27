module Main where
{-
    Week 07 - Folds and Monoids
              supplied module
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-}


import StringBuffer
import Editor

main = runEditor editor buf

buf = unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
