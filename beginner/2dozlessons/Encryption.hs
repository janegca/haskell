-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 16 - Modules as Libraries

{-
    cipher function encodes a message in 5 steps:
    
    1. group characters in original message into blocks
    2. convert each block to a symbol in the cipher alphabet
    3. shift the cipher-alphabet symbol by the amount denoted by the key
    4. convert each (shifted) cipher-alphabet symbol into a block of 
       characters
    5. string the blocks together into one string, which is the encoded 
       message (ciphertext)
-}

module Encryption
    (encipher, decipher)
where

import EncryptionUtilities
import SequenceUtilities

encipher, decipher :: Int -> String -> String -> String
encipher blockSize key = cipher blockSize (symFromBlock key)

decipher blockSize key = cipher blockSize (- symFromBlock key)

cipher :: Int -> Integer -> String -> String
cipher blockSize keyAsInteger = 
    concat .                    -- de-block
    map blockFromSym .          -- back to blocks (from symbols)
    map shiftSym .              -- encipher symbols
    map symFromBlock .          -- convert to cipher-alphabet symbol
    blocksRigid blockSize ' '   -- form blocks
    where
        shiftSym n   = (n + keyAsInteger) `mod` alphabetSize
        alphabetSize = numeralBase^blockSize
