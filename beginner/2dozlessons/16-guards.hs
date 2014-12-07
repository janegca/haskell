-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 16 - Definitions with Alternatives 
--      introduces 'guards'

import Data.Char (isUpper)

{-
    Simple cipher code used by Julius Ceaser, each letter
    is replaced by one 3 letters forward from it; wraps
    when it reaches the end of the alphabet.
    
    Example Usage:
    
        *Main> cipherJulius "VENIVIDIVICI"
        "YHQLYLGLYLFL"
        
        *Main> decipherJulius it
        "VENIVIDIVICI"
        
    Implementation Notes:
    
        the curried functions can leave off the argument
        i.e.    cipherJulius xs = map(shiftAZ 3) xs
             == cipherJulius    = map(shiftAz 3)
             
             because
                g y z x = anyFormula x
             is equivalent to
                g y z = anyFormula
                
        Any two formulae are equal if, given the same inputs,
        they return the same outputs. (Mathematical concept
        of functions that translates directly into Haskell).
        
        When this form of writing functions is use it is 
        important to USE EXPLICIT TYPE DEFINITIONS to avoid
        ambiguities.
        
        Enum methods are used rather than straight conversions
        of letters using Data.Char ord and chr as we need the
        position of each letter with reference to the 'A'
        ordinal; not simply the letter ordinal itself
                  
-}

cipherJulius :: String -> String
cipherJulius = map(shiftAZ 3)           -- curried function

-- Note: negative numbers in Haskell MUST be wrapped in parentheses
--       otherwise the minus sign can be confused with the subtraction 
--       operator
decipherJulius :: String -> String
decipherJulius = map(shiftAZ (-3))      -- curried function

-- encode a character
shiftAZ n c = ltrFromInt((intFromLtr c + n) `mod` 26)

{-
    GUARDS 
    
    Following methods offer alternative processing paths
    through the use of 'guards'.

    Guards are predicate functions (functions evaluating to True 
    or False) which are used to test for required conditions. 
    If the predicate is:
    
        True  - execute the formula to the right of the '='
        False - fall through to the next line of code
        
    If all tests are false, the code falls through to 'otherwise' 
    and the formula it equates to is executed.
    
    A function can have multiple guards
-}

-- translate a Char to its ASCII value
intFromLtr :: Char -> Int
intFromLtr c 
    | isUpper c  = fromEnum c - fromEnum 'A'
    | otherwise  = error "not uppercase"

-- translate an ASCII value to a Char
ltrFromInt :: Int -> Char
ltrFromInt n
    | n >= fromEnum 'A' = toEnum(n + fromEnum 'A')
    | otherwise = error "not uppercase"

{-
    map
        the equivalent of a list comprehension:
            map f xs = [f x | x <- xs]                        
-}
{-
    The Roman alphabet did not include the letters J, U or W
    Rewrite the Caesar cipher to use the Roman alphabet.    
    
    Example:
        *Main> cipherCaesar "VENEVIDIVICI"
        "ZHQHZMGMZMFM"
        *Main> decipherCaesar it
        "VENEVIDIVICI"    
-}
cipherCaesar :: String -> String
cipherCaesar = map(shiftRomanLetter 3)

shiftRomanLetter :: Int -> Char -> Char
shiftRomanLetter n c = romFromInt((intFromRom c + n) `mod` 23)

intFromRom :: Char -> Int
intFromRom c 
    | c >= 'A' && c <= 'I'  = n         
    | c >= 'K' && c <= 'T'  = n - 1     -- skip J
    | c == 'V'              = n - 2     -- skip U
    | c >= 'X' && c <= 'Z'  = n - 3     -- skip W
    | otherwise             = error ("not a Roman letter")
    where
        n = fromEnum c - fromEnum 'A'  

romFromInt :: Int -> Char
romFromInt n
    | n >= intFromRom 'A' && n <= intFromRom 'I' = romChr n
    | n >= intFromRom 'K' && n <= intFromRom 'T' = romChr (n+1)
    | n == intFromRom 'V'                        = romChr (n+2)
    | n >= intFromRom 'X' && n <= intFromRom 'Z' = romChr (n+3)
    | otherwise  = error ("not a Roman letter")
    where 
        romChr code = toEnum(code + fromEnum 'A')
   
decipherCaesar :: String -> String
decipherCaesar = map(shiftRomanLetter (-3))
   
