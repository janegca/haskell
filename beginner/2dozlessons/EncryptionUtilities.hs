-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 16 - Modules as Libraries

{-
    Example:
    *EncryptionUtilities> integerFromNumeral numeralBase (
       map integerCodeFromChar "hello")
    6616708126
-}

module EncryptionUtilities (
    symFromBlock,
    blockFromSym,
    numeralBase) where
    
import Numerals

-- public (exported) functions
symFromBlock :: String -> Integer
symFromBlock = 
    integerFromNumeral numeralBase . map integerCodeFromChar

blockFromSym :: Integer -> String
blockFromSym =
    map charFromIntegerCode . numeralFromInteger numeralBase

numeralBase :: Integer
numeralBase = fromIntegral(maxCode - minCode + 1 + numExtraCodes)

-- private functions
integerCodeFromChar :: Char -> Integer
integerCodeFromChar c
    | fromEnum c >= minCode = fromIntegral(fromEnum c - minCode + 2)
    | otherwise             = fromIntegral(fromEnum c - fromEnum '\t')

charFromIntegerCode :: Integer -> Char
charFromIntegerCode code
    | intCode >= numExtraCodes = toEnum(intCode + minCode - 2)
    | otherwise                = toEnum(intCode + fromEnum '\t')
    where
        intCode = fromIntegral code

maxCode, minCode, numExtraCodes :: Int
-- set of codes = {tab, newline, toEnum 32 ... toEnum 126}
maxCode       = 126
minCode       =  32
numExtraCodes = 2


