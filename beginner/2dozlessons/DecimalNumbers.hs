-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 15 - Modules

module DecimalNumbers (  
    -- exported functions
    integerFromDecimalNumeral,
    decimalNumeralFromInteger) where
    
-- public (exported) functions    
integerFromDecimalNumeral ds = (horner10 . reverse) ds

decimalNumeralFromInteger x =
    reverse [d | (s,d) <- takeWhile (/= (0,0)) (sdPairs x)]
    
-- private functions    
horner10 ds = foldr multAdd 0 ds

multAdd d s = d + 10*s

sdPairs x = iterate nextDigit (x `divMod` 10)

nextDigit(xShifted, d) = xShifted `divMod` 10    

