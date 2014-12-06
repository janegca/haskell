-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 15 - Modules

module Numerals (
    integerFromNumeral,
    numeralFromInteger) where
    
import PolynomialEvaluation( horner )

integerFromNumeral b x = (horner b . reverse) x  

numeralFromInteger b x
    = reverse [d | (s,d) <- takeWhile (/= (0,0)) (sdPairs b x)]
    
sdPairs b x = iterate (nextDigit b) (x `divMod` b)

nextDigit b (xShifted, d) = xShifted `divMod` b   

