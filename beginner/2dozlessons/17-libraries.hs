-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 16 - Modules as Libraries
--      using modules to create an encryption library
--      files supplied: SequenceUtilities
--      files created:  Encryption, EncryptionUtilities

{-
    An example of creating and using a 'library' of modules
    dedicated to a specific use: encoding and decoding
    cipher text
-}
import Encryption

maximDijkstra, ciphertext, plaintext :: String
maximDijkstra =
    "Besides a mathematical inclination, an exceptionally\n" ++
    "good mastery of one's native tongue is the most vital\n" ++
    "asset of a competent programmer.\n"
    
ciphertext = encipher blockSize key maximDijkstra
plaintext  = decipher blockSize key ciphertext

key :: String
key = "computing science"

blockSize :: Int
blockSize = 10
