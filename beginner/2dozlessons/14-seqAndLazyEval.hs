-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 14 - Truncating Sequences and Lazy Evaluation

{-
    Haskell provides four functions for working with part of a
    sequence: take, drop, takeWhile, dropWhile
    
    take n xs  - will grab 'n' elements of the sequence 'xs'
    drop n xs  - will drop the first 'n' elements of the sequence 'xs'
                 and return the remaining elements
                 
    Example:
        ghci> take 3 [1..7]
        [1,2,3]
        ghci> drop 3 [1..7]
        [4,5,6,7]
        ghci> (take 3 . drop 2) [1..7]
        [3,4,5]
        ghci> take 3 [1,2]
        [1,2]
        ghci> drop 3 []
        []        
     
    Putting the functions to use
  
-}
allDigitsInNumeralStartingFromUnitsPlace :: Integral n => n -> [n]
allDigitsInNumeralStartingFromUnitsPlace x
    = [d | (s,d) <- sdPairs x]
    where
        sdPairs :: Integral n => n -> [(n,n)]
        sdPairs x = iterate nextDigit (x `divMod` 10)
        
        nextDigit :: Integral n => (n, a) -> (n, n)
        nextDigit(xShifted, d) = xShifted `divMod` 10

lastFourDecimalDigits :: Integral n => n -> [n]        
lastFourDecimalDigits x
    = (reverse . take 4 . allDigitsInNumeralStartingFromUnitsPlace) x
    
{-
    Example:
    
        ghci> lastFourDecimalDigits 1937
        [1,9,3,7]
        ghci> lastFourDecimalDigits 486
        [0,4,8,6]
        ghci> lastFourDecimalDigits 68009
        [8,0,0,9]    

    'takeWhile' and 'dropWhile' act similar to their counterparts
    'take' and 'drop' but rather than operate solely by counting
    off elements they look for conditions to stop or start.
    
    The condition (predicate) is one that evaluates to True or
    False and it is passed in as the first argument. As long
    as the elements pass the condition, takeWhile will continue
    to take them or dropWhile will continue to drop them.
    
    Examples:
        ghci> takeWhile odd [3, 1, 4, 1, 5, 9, 2, 6]
        [3,1]
        ghci> dropWhile odd [3, 1, 4, 1, 5, 9, 2, 6]
        [4,1,5,9,2,6]
        ghci> dropWhile (<5) [3, 1, 4, 1, 5, 9, 2, 6]
        [5,9,2,6]
        ghci> takeWhile (<5) [3, 1, 4, 1, 5, 9, 2, 6]
        [3,1,4,1]    
        
    LAZY EVALUATION
        In Haskell, nothing is computed until it is needed
        so, for 'takeWhile' only the elements generated are
        those up until the predicate returns its first False
        
        Note - need to be careful when using dropWhile with
               infinite sequences as it will try to return
               everything after the first element that fails
               the predicate condition
               
    The 'sdPairs' function below uses 'iterate' to generate an
    infinite list; rather than limiting ourselves to functions
    that can only take a specified number of digits, we can use
    'takeWhile' to take all relevant digits regardless of the
    number size.
-}    

decimalDigitsFromInteger x
    = reverse [d | (s,d) <- takeWhile (/= (0,0)) (sdPairs x)]
    where
        sdPairs :: Integral n => n -> [(n,n)]
        sdPairs x = iterate nextDigit (x `divMod` 10)
        
        nextDigit :: Integral n => (n, a) -> (n, n)
        nextDigit(xShifted, d) = xShifted `divMod` 10
    
{-

    Examples:
        ghci> decimalDigitsFromInteger 486
        [4,8,6]
        ghci> decimalDigitsFromInteger 1978
        [1,9,7,8]
        ghci> decimalDigitsFromInteger 2341411
        [2,3,4,1,4,1,1]    
-}        
