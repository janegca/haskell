{- CIS914 - Week 2 Working with Lists
   Source: No lecture material
   
   Homework covered
        Local variables
        let expressions
        where clauses
        Haskell Layout
        Accumulators
-}

-- use 'let' to define local variables whose scope is
-- the defining expression (ie within the 'let' espression itself)
strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs 
                   in len_rest + 1
                   
-- use 'where' to define a local variable whose scope is over
-- a multilple-branch guarded expression
frob :: String -> Char
frob [] = 'a' -- len is NOT in scope here
frob str
    | len > 5 = 'x'
    | len < 3 = 'y'
    | otherwise = 'z'
    where
        len = strLength str      

-- recursion with accumulator
-- sum until the total is > 20
sumTo20 :: [Int] -> Int
sumTo20 nums = sumTo20Helper 0 nums -- the acc. starts at 0

sumTo20Helper :: Int -> [Int] -> Int
sumTo20Helper acc [] = acc -- empty list: return the accumulated sum
sumTo20Helper acc (x:xs)
    | acc >= 20 = acc
    | otherwise = sumTo20Helper (acc + x) xs
    
    

        
