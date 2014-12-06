-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 15 - Modules

-- Original DecimalNumbers modified to use Numerals module
module DecimalNumerals (  
    -- exported functions
    integerFromDecimalNumeral,
    decimalNumeralFromInteger) where
    
import Numerals(integerFromNumeral, numeralFromInteger)
    
-- public (exported) functions    
integerFromDecimalNumeral ds = integerFromNumeral 10 ds

decimalNumeralFromInteger x = numeralFromInteger 10 x
    
