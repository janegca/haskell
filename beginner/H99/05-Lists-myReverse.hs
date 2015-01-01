{-
    Problem 5
    
    (*) Reverse a list.

    Example in Haskell:

    Prelude> myReverse "A man, a plan, a canal, panama!"
    "!amanap ,lanac a ,nalp a ,nam A"
    Prelude> myReverse [1,2,3,4]
    [4,3,2,1]
    
-}
myReverse :: [a] -> [a]     -- expensive
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- other methods from H99 site

myReverse_a :: [a] -> [a]               -- Prelude definition, best
myReverse_a =  foldl (flip (:)) []

myReverse_b :: [a] -> [a]               -- less costly
myReverse_b list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

myReverse_c :: [a] -> [a]               -- difficult to read
myReverse_c xs = foldr (\x fId empty -> fId (x : empty)) id xs []
    
-- tests -----------------------------------------------------------------
testCharLst f = f "A man, a plan, a canal, panama!" ==
                  "!amanap ,lanac a ,nalp a ,nam A"
                
testIntLst  f = f [1,2,3,4] == [4,3,2,1]              

tests = testIntLst myReverse   && testCharLst myReverse
     && testIntLst myReverse_a && testCharLst myReverse_a
     && testIntLst myReverse_b && testCharLst myReverse_b
     && testIntLst myReverse_c && testCharLst myReverse_c
  
          