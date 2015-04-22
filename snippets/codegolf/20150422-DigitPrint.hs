{-
    Code Golf - Digit Print
    
    This week's challenge is pretty straight forward. Given an input 
    (any single digit number), you'll output all of the numbers between 
    -100 and 100 (inclusive) containing that digit. For instance:

    INPUT: 2

    OUTPUT: -92 -82 -72 -62 -52 -42 -32 -29 -28 -27 -26 -25 -24 -23 -22 
            -21 -20 -12 -2 2 12 20 21 22 23 24 25 26 27 28 29 32 42 52 62 
            72 82 92

    Source:
    http://java.dzone.com/articles/code-golf-digit-print
    
    digitPrint  has 98 characters (excl. blanks and fn name)
    digitPrint' has 66 chars (excl. blanks and fn name)
-}
digitPrint = do d <- getChar
                print (s d)
    where s x = [ n | n <- [-100..100], elem x (show n)]
    
digitPrint' = getChar >>= \d -> 
              print [ n | n <- [-100..100], elem d (show n)]  