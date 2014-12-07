-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 19 - Interactive Programs with File I/O


import Encryption(encipher)     -- used by 'encryptFile'

-- get a line of text from the user and write it to a file
main =
    do
        putStr(unlines["Enter one line."])
        lineFromKeyboard <- getLine
        
        writeFile filename lineFromKeyboard
        
        putStr("Entered line written to file \"" ++ filename ++ "\" \n")
    where
        filename = "oneLiner.txt"

-- write data directly to a file        
main' =
    writeFile "restaurant.dat" (unlines pepes)
    where
        pepes = ["Pepe Delgados", "752 Asp", "321-6232"]
        
{-
    ENCRYPT A FILE
    
    1. Ask the user for the name of a plain text file to be encrypted
    2. Ask the user for an encryption key
    3. Read in the plain text file
    4. Encrypt the file and write it out to a file with .ctx extension
    5. Let the user know we're finished
    
    Example:
    
        *Main> encryptFile
        Enter name of file containing plaintext: sussman.txt
        Thank you
         ... will read plaintext from sussman.txt
         ... and write ciphertext to sussman.txt.ctx
        Enter key: functionalprogramming
        Thank you ...
         ... will use key, then throw into bit-bucket
        Reading plaintext from sussman.txt
         ... computing ciphertext
         ... ciphertext written to sussman.txt.ctx    

-}

encryptFile =
    do
        filename <- getFilename
        confirmFilename filename
        
        key <- getKey
        confirmKey
        
        putStr(msgReading filename)
        
        plaintext <- readFile filename
        putStr msgComputing
        
        writeFile (outFile filename) (encipher blockSize key plaintext)
        putStr (msgSignOff(outFile filename))

getFilename =
    do
        putStr msgEnterFilename
        filename <- getLine
        return filename

confirmFilename filename = putStr (msgThxForFilename filename)

getKey =
    do
        putStr msgEnterKey
        key <- getLine
        return key

confirmKey = putStr msgThxForKey

msgEnterFilename = "Enter name of file containing plaintext: "
msgThxForFilename filename =
    unlines[
    "Thank you",
    " ... will read plaintext from " ++ filename,
    " ... and write ciphertext to " ++ outFile filename]

msgEnterKey = "Enter key: "

msgThxForKey =
    unlines[
    "Thank you ...",
    " ... will use key, then throw into bit-bucket"]

msgReading filename =
    unlines["Reading plaintext from " ++ filename]

msgComputing = unlines[" ... computing ciphertext"]

msgSignOff filename =
    unlines[" ... ciphertext written to " ++ filename]

outFile filename = filename ++ ".ctx"

blockSize :: Int
blockSize = 10    
                
