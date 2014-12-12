-- Informatics 1 - Functional Programming 
-- Tutorial 2 - The Caesar Cipher
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/tutorial2.zip
--
{-

    The Caesar Cipher
    
    When we talk about cryptography these days, we usually refer to 
    the encryption of digital messages, but encryption actually predates 
    the computer by quite a long period. One of the best examples
    of early cryptography is the Caesar cipher, named after Julius Caesar 
    because he is believed to have used it, even if he didn't actually 
    invent it. The idea is simple: take the message you want to
    encrypt and shift all letters by a certain amount between 0 and 26 
    (called the offset). For example: encrypting the sentence 
    "THIS IS A BIG SECRET" with shifts of 5, would result in "YMNX NX
    F GNL XJHWJY".
    
    In this exercise you will be implementing a variant of the Caesar 
    cipher. 
    
    Encrypting text
    
    A character-by-character cipher such as a Caesar cipher can be 
    represented by a key, a list of pairs. Each pair in the list 
    indicates how one letter should be encoded. For example, a cipher
    for the letters  A{E could be given by the list
    
    [('A', 'C'), ('B', 'D'), ('C', 'E'), ('D', 'A'), ('E', 'B')] .
    
    Although it's possible to choose any letter as the cipher text for 
    any other letter, this tutorial deals mainly with the type of cipher 
    where we encipher each letter by shifting it the same number of spots
    around a circle, for the whole English alphabet.

-}

import Data.Char
import Data.List
import Test.QuickCheck


{- 1.
    We can rotate a list by taking some items off the front of it and 
    putting them on the end. For example:
    
        Main> rotate 3 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "DEFGHIJKLMNOPQRSTUVWXYZABC"
        
    Complete the function rotate :: Int -> [Char] -> [Char]. When
    given a number n greater or equal to 0 and smaller or equal the 
    length of the input list, your function should rotate the list by 
    n items. Your function should return an error if the number
    n is negative or too large.
-}
rotate :: Int -> [Char] -> [Char]
rotate n xs | n >= 0 && n <= lenXs = (drop n xs) ++ (take n xs)
            | otherwise = error ("n is negative or to large")
            where
                lenXs :: Int
                lenXs = length xs

{- 2.
    (a) What precisely does prop_rotate test?
            random strings
            
    (b) Your function rotate can produce an error if the Int provided 
       is negative or too large. How does prop_rotate avoid triggering 
       this error?
            checks the length of the string and defines the
            'shift' to fall within 0 and the length
-}
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

{- 3. 

    Using the function rotate from the previous question, write a 
    function 
        makeKey :: Int -> [(Char, Char)]
    that returns the cipher key with the given offset. See above for 
    the description of how the cipher key is represented as a list of 
    pairs. Example:
    
        Main> makeKey 5
        [('A','F'),('B','G'),('C','H'),('D','I'),('E','J'),('F','K'),
        ('G','L'),('H','M'),('I','N'),('J','O'),('K','P'),('L','Q'),
        ('M','R'),('N','S'),('O','T'),('P','U'),('Q','V'),('R','W'),
        ('S','X'),('T','Y'),('U','Z'),('V','A'),('W','B'),('X','C'),
        ('Y','D'),('Z','E')]

    The cipher key should show how to encrypt all of the uppercase English
    letters, and there should be no duplicates: each letter should appear 
    just once amongst the pairs' first components (and just once amongst
    the second components).
    
-}
makeKey :: Int -> [(Char, Char)]
makeKey n = zip alphabet (rotate n alphabet)
    where 
        alphabet :: String
        alphabet = ['A'..'Z']

{- 4.
    Write a function
        lookUp :: Char -> [(Char, Char)] -> Char
        
    that finds a pair by its first component and returns that pair's 
    second component. When you try to look up a character that does 
    not occur in the cipher key, your function should leave it unchanged. 
    Examples:
    
        Main> lookUp 'B' [('A', 'F'), ('B', 'G'), ('C', 'H')]
        'G'
        Main> lookUp '9' [('A', 'X'), ('B', 'Y'), ('C', 'Z')]
        '9'

    Use list comprehension, then write an equivalent function lookUpRec 
    using recursion. Test that the two functions are equivalent with a 
    test function prop_lookUp.
-}
--  modified based on provided solution
lookUp :: Char -> [(Char, Char)] -> Char
lookUp chr keys = head ([ k | (a,k) <- keys, a == chr ] ++ [chr])

{- my solution
lookUp chr keys = if key /= [] then (head key) else chr
    where
        key = [ k | (a,k) <- keys, a == chr ]
-}        

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec chr [] = chr
lookUpRec chr ( (a,k) :ks)
    | chr == a  = k
    | otherwise = lookUpRec chr ks

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp chr keys = lookUp chr keys == lookUpRec chr keys

{- 5.
    Write a function
        encipher :: Int -> Char -> Char
        
    that encrypts the given single character using the key with the 
    given offset. For example:
    
        Main> encipher 5 'C'
        'H'
        Main> encipher 7 'Q'
        'X'
-}
encipher :: Int -> Char -> Char
encipher shift chr = lookUp chr (makeKey shift)

{- 6.
    Text encrypted by a cipher is conventionally written in uppercase
    and without punctuation. Write a function
        normalize :: String -> String

    that converts a string to uppercase, removing all characters other 
    than letters and digits (remove spaces too). Example:
        Main> normalize "July 4th!"
        "JULY4TH"
-}
normalize :: String -> String
normalize str = [ toUpper ltr | ltr <- str
                              , (isAlpha ltr) || (isDigit ltr) ]

{- 7.
    Write a function
        encipherStr :: Int -> String -> String
        
    that normalizes a string and encrypts it, using your functions 
    normalize and encipher. Example:
        Main> encipherStr 5 "July 4th!"
        "OZQD4YM"
-}
encipherStr :: Int -> String -> String
encipherStr shift str = [ encipher shift c | c <- normalize str]

{-
    Decoding a message
    
    The Caesar cipher is one of the easiest forms of encryption to break. 
    Unlike most encryption schemes commonly in use today, it is 
    susceptible to a simple brute-force attack of trying all the possible
    keys in succession. The Caesar cipher is a symmetric key cipher: 
    the key has enough information within it to use it for encryption 
    as well as decryption.
-}

{- 8.
    Decrypting an encoded message is easiest if we transform the key
    first. Write function
        reverseKey :: [(Char, Char)] -> [(Char, Char)]
        
    to reverse a key. This function should swap each pair in the given 
    list. For example:
        Main> reverseKey [('A', 'G'), ('B', 'H') , ('C', 'I')]
        [('G', 'A'), ('H', 'B') , ('I', 'C')]

    First use list comprehension, then write an equivalent function
    reverseKeyRec using recursion. Check that your two functions are 
    equivalent with a test function prop_reverseKey.
-}
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey keys = [ (k,a) | (a,k) <- keys]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((a,k):ks) = (k,a) : reverseKeyRec ks

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey keys = reverseKey keys == reverseKeyRec keys

{- 9.
    Write the functions
        decipher :: Int -> Char -> Char
        decipherStr :: Int -> String -> String
        
    that decipher a character and a string, respectively, by using the 
    key with the given offset.
    
    Your function should leave digits and spaces unchanged, but remove 
    lowercase letters and other characters. For example:
        Main> decipherStr 5 "OZQD4YM"
        "JULY4TH"
-}
decipher :: Int -> Char -> Char
decipher shift chr = lookUp chr (reverseKey(makeKey shift))

decipherStr :: Int -> String -> String
decipherStr shift str = [ decipher shift c | c <- str
                                           ,  isDigit c 
                                           || isUpper c
                                           || isSpace c ]

{-
    Breaking the encryption
    
    One kind of brute-force attack on an encrypted string is to decrypt 
    it using each possible key and then search for common English letter 
    sequences in the resulting text. If such sequences are discovered
    then the key is a candidate for the actual key used to encrypt the
    plain text. For example, the words "the" and "and" occur very 
    frequently in English text: in the Adventures of Sherlock Holmes, 
    "the" and "and" account for about one in every 12 words, and there 
    is no sequence of more than 150 words without either "the" or "and".
    
    The conclusion to draw is that if we try a key on a sufficiently long 
    sequence of text and the result does not contain any occurrences of
    "the" or "and" then the key can be discarded as a candidate.
-}                                           
{- 10.
    Write a function 
        contains :: String -> String -> Bool t
    that returns True if the first string contains the second as a 
    substring (this exercise is the same as the last of the optional
    exercises of the previous tutorial).
    
        Main> contains "Example" "amp"
        True
        Main> contains "Example" "xml"
        False
-}
contains :: String -> String -> Bool
contains xs ys =  [] /= [ True | x <- suffixes xs, isPrefixOf ys x]
    where
        suffixes :: String -> [String]
        suffixes xs = [drop i xs | i <- [0.. length xs]]
        
-- provided solution was this recursive definition
containsRec :: String -> String -> Bool
containsRec _ [] = True
containsRec [] _ = False
containsRec str substr = 
    isPrefixOf substr str || containsRec (tail str) substr
        
{- 11.
    Write a function
        candidates :: String -> [(Int, String)]
        
    that decrypts the input string with each of the 26 possible keys and, 
    when the decrypted text contains "THE" or "AND", includes the 
    decryption key and the text in the output list.
        Main> candidates "DGGADBCOOCZYMJHZYVMTOJOCZHVS"
        [(5,"YBBVYWXJJXUTHECUTQHOJEJXUCQN"),
        (14,"PSSMPNOAAOLKYVTLKHYFAVAOLTHE"),
        (21,"ILLFIGHTTHEDROMEDARYTOTHEMAX")]
-}
candidates :: String -> [(Int, String)]
candidates txt = [ (shift, str) | (shift, str) <- decryptedTxt txt
                                ,  contains str "THE"
                                || contains str "AND" ]
    where
        decryptedTxt :: String -> [(Int, String)]
        decryptedTxt txt = [ (n, decipherStr n txt)| n <- [1..26] ]
                               
-- Optional Material
{-
    As you have seen in the previous section, the Caesar Cipher is not 
    a very safe encryption method. In this section, security will be 
    upgraded a little.
-}
{- 12.
    Write a function splitEachFive :: String -> [String] that splits a 
    string into substrings of length five. Fill out the last part with 
    copies of the character 'X' to make it as long as the others.

        Main> splitEachFive "Secret Message"
        ["Secre", "t Mes", "sageX"]
-}
-- modified after reviewing provided solution
splitEachFive :: String -> [String]
splitEachFive xs 
    | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
    | otherwise     = [ take 5 (xs ++ repeat 'X') ]

{- my original solution
splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive msg = split pMsg
    where
        pLen :: Int
        pLen = 5 - (length msg) `mod` 5
        
        pMsg :: String
        pMsg = msg ++ take pLen "XXXXX"
        
        split :: String -> [String]
        split []      = []
        split "XXXXX" = []
        split str     = [take 5 str] ++ split (drop 5 str)
-}        
        
{- 13.
    The library function transpose switches the rows and columns of a 
    list of lists:
        Main> transpose ["123","abc","ABC"]
        ["1aA","2bB", "3cC"]
        Main> transpose ["1","22","333"]
        ["123","23","3"]
        
    If the rows in a list of lists are of the same length, transposing 
    it twice returns the original one. Use your splitEachFive function 
    to write a quickCheck property to test this. Also,
    show with an example that this is not always the case when the 
    rows are of different lengths.
-}
prop_transpose :: String -> Bool
prop_transpose msg = splitEachFive msg ==
                     transpose(transpose (splitEachFive msg))
          
-- this does not return the original list          
ex13 = transpose(transpose ["1","22","333"]) == ["1","22","333"]

{- 14.
    Write a function 
        encrypt :: Int -> String -> String 
    
    that encrypts a string by first applying the Caesar Cipher, then 
    splitting it into pieces of length five, transposing, and putting 
    the pieces together as a single string.
-}
encrypt :: Int -> String -> String
encrypt n msg = concat(transpose (splitEachFive (encipherStr n msg)))

{- 15.
    Write a function to decrypt messages encrypted in the way above.
    
    Hint: The last action of the previous function is to put the 
          transposed list of strings back together. You will need a 
          helper function to undo this (it is not splitEachFive).
-}

decrypt :: Int -> String -> String
decrypt shift eTxt = decipherStr shift (concat(transpose(splitIn5 eTxt)))
        where
            splitIn5 :: String -> [String]
            splitIn5 []   = []
            splitIn5 cTxt = split cTxt
                where
                    n :: Int
                    n = (length cTxt) `div` 5
                    
                    split :: String -> [String]
                    split [] = []
                    split msg = [take n msg] ++ split( drop n msg )

-- Challenge (Optional)
{-
    While the strengthened encryption thwarts a cracker searching for 
    candidate substrings in the cipher text, it is of little help against 
    a cracker looking at the cipher-text letter-by-letter. If the cracker
    can determine the relationship between a letter and its enciphered 
    counterpart, she can compute the offset used in enciphering. Now it 
    turns out that in English prose, certain letters are more common
    than others, so if we compare the relative frequencies of enciphered 
    letters with those of prose, we can often determine the Caesar offset.
    For instance,
        Main> encypt 12 "Secret Message"
        "EFMQYSOQQDEXQEX"
    The most frequent letter in the above cipher-text is `Q'. This letter 
    lies 12 places to the right in the  alphabet from the letter `E', and 
    the letter `E' is the most common letter in English prose.
    
-}
{- 16.
    To perform decipherings based on this method, first write a function
        countFreqs :: String -> [(Char, Int)] 
    to compute the frequency of each character in the input string:
    
        Main> countFreqs "Secret Message"
        [('S',1),('e',4),('c',1),('r',1),('t',1)
        ,(' ',1),('M',1),('s',2),('a',1),('g',1)]
-}
countFreqs :: String -> [(Char, Int)]
countFreqs msg = removeDups [] (zip msg [ count c msg | c <- msg])
    where
        count :: Char -> String -> Int
        count c str = sum [ 1 | x <- str, x == c]
        
        removeDups :: Eq a => [a] -> [a] -> [a]
        removeDups acc [] = acc
        removeDups acc (x:xs) 
            | elem x acc = removeDups acc xs
            | otherwise  = removeDups (acc ++ [x]) xs
      
{- 17
    Next, write a function 
        freqDecipher :: String -> [String] 
    
    which tries to decrypt a string encrypted using encrypt. It should 
    use countFreqs to rank the candidates which will decipher to `E', 
    from most frequent to least, and then use decrypt to produce a ranked
    list of potential decipherings.
        Main> freqDecipher (encrypt 20 "To be or not to be")
        ["JERUEHDEJJERUTT","EZMPZCYZEEZMPOO","WREHRUQRWWREHGG"
        ,"TOBEORNOTTOBEDD",...]

-}
-- added 'mod 26' after viewing solution
freqDecipher :: String -> [String]
freqDecipher ctxt = [ decrypt ((ord ltr - ordE) `mod` 26) ctxt |
                        (ltr, _) <- rankedTxt ]
    where
        ordE = ord 'E'
        rankedTxt = sortBy rankFreqs (countFreqs ctxt)        
        rankFreqs (_, c1) (_, c2) = compare c2 c1
