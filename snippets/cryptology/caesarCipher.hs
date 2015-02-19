-- Caesar Cipher

{- References:
    [TDSL]: Two Dozen Short Lessons in Haskell by Rex Page
-}

import Data.Char (isUpper)
import Characters

{-
    Simple cipher code used by Julius Caesar, each letter
    is replaced by one 3 letters forward from it; wraps
    when it reaches the end of the alphabet.
    
    Example Usage:
    
        *Main> cipherJulius "VENIVIDIVICI"
        "YHQLYLGLYLFL"
        
        *Main> decipherJulius it
        "VENIVIDIVICI"
-}
cipherJulius :: String -> String
cipherJulius = map(shiftAZ 3)        

decipherJulius :: String -> String
decipherJulius = map(shiftAZ (-3)) 

-- encode a character
shiftAZ n c = ltrFromInt((intFromLtr c + n) `mod` 26)

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
