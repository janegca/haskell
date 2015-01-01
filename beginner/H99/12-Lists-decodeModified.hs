{-

    Problem 12
    
    (**) Decode a run-length encoded list.

    Given a run-length code list generated as specified in problem 11. 
    Construct its uncompressed version.

    Example in Haskell:

    P12> decodeModified 
           [Multiple 4 'a',Single 'b',Multiple 2 'c',
            Multiple 2 'a',Single 'd',Multiple 4 'e']
    "aaaabccaadeeee"    
-}

-- run length encoded list data type from Problem 11
data Encoded a = Single a
               | Multiple Int a
    deriving (Eq, Show)

decodeModified :: [Encoded a] -> [a]
decodeModified []                    = []
decodeModified ((Single v) : xs)     = v : decodeModified xs
decodeModified ((Multiple n v) : xs) = (replicate n v) ++
                                       decodeModified xs
        
-- other methods from H99 site -------------------------------------------
decodeModified_b :: [Encoded a] -> [a]
decodeModified_b = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x        
      
-- tests ------------------------------------------------------------------
testLst f =  f [Multiple 4 'a',Single 'b',Multiple 2 'c',
                Multiple 2 'a',Single 'd',Multiple 4 'e']
             == "aaaabccaadeeee"
             
tests = testLst decodeModified   &&  testLst decodeModified_b          