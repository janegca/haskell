-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 15 - Modules
--      Files written for this lesson:
--          numconv.hs
--          PolynomialEvaluation.hs
--          Numerals.hs
--          DecimalNumerals.hs
--          DecimalNumbers.hs

{-
    A module is a script that makes some or all of its entities
    visible to other scripts. The visible entities are declared
    (exported) in the module header.
    
    By convention, modules are limited one to a file and the file
    is given the same name as the module.
    e.g. the module 'DecimalNumbers' is stored in a file named
         DecimalNumbers.hs
         
    An exception is made for the main module which is generally
    given a name indicating its purpose.
    
    A script may import another module; it will only have access
    to that module's public (exported) entities.
    
    An external script can import all or only a few of the import
    modules public entities:
    
        import moduleName   -- imports all public entities
        import moduleName (name1, name2) -- imports 2 pubic entities
        import moduleName hiding (nameOfEntityToHide) 
        
-}

import DecimalNumbers

{-
    Example:
    
        *Main> decimalNumeralFromInteger 1993
        [1,9,9,3]
        *Main> integerFromDecimalNumeral [1,9,9,3]
        1993
        *Main> decimalNumeralFromInteger 1993
        [1,9,9,3]
        *Main> (integerFromDecimalNumeral . decimalNumeralFromInteger) 1993
        1993
        *Main> nextDigit(199,3)

        <interactive>:12:1: Not in scope: `nextDigit'    

-}

{-  See full example by loading 'numconv' into ghci and trying
    out some number conversions -}
