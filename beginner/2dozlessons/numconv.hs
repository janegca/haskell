-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 15 - Modules

-- a main module for converting numbers

import Numerals(integerFromNumeral, numeralFromInteger )
import DecimalNumerals(decimalNumeralFromInteger)

{-
    Example:
    
        *Main> decimalNumeralFromInteger 2001
        [2,0,0,1]
        
        *Main> numeralFromInteger 2 2001
        [1,1,1,1,1,0,1,0,0,0,1]
        
        *Main> numeralFromInteger 60 138
        [2,18]    
    
            - 138 minutes = 2 hours 18 minutes
            
        *Main> numeralFromInteger 60 11697   -- seconds to h,m,s
        [3,14,57]
        
        *Main> numeralFromInteger 12 68      -- inches to feet
        [5,8]
        
        *Main> integerFromNumeral 5280 [6,1000] 
        32680
        
            -- 6 miles and 1000 feet is a cruising altitude of 32680
            -- feet
            
            walkthrough:
            
            integerFromNumeral 5280 [6, 1000]
            => (horner 5280 . reverse) [6, 1000]
            => (horner 5280) [1000, 6]
            => foldr (mulAdd 5280) 0 [1000, 6]
            => 1000 (multAdd 5280) (6 (multAdd 5280) 0)
            => 1000 (multAdd 5280) (6 + 5280 * 0)
            => 1000 (multAdd 5280) 6
            => 1000 + 5280 * 6
            => 1000 + 31680
            => 32680
            
            
        
            
-}
