-- Chapter 1 Review Exercises

-- Ex 15 write a function that takes and prints a Maybe value
showMaybe :: Show a => Maybe a -> String
showMaybe Nothing  = ""
showMaybe (Just x) = (show x)


{-
    Ex 16
    
    A Bit is a '0' or '1'
    A Word is a list of bits that represent a binary number
    bitOr and birAnd are the equivalents of Bit 'or' and 'and'
    
    Write a function bitwiseAnd that takes two Words and creates a third
    Word that is the bitwise and of the two Words.
    
    Examples:
    
        bitwiseAnd [1,0,0] [1,0,1]
            => [bitAnd 1 1, bitAnd 0 0, bitAnd 0 1]
            => [1,0,0]
        
        bitwiseAnd [0,0,0] [1,1,0]
            => [0,0,0]
    
-}
bitOr :: Int -> Int -> Int
bitOr 0 0 = 0
bitOr x y = 1

bitAnd :: Int -> Int -> Int
bitAnd 1 1 = 1
bitAnd x y = 0

bitwiseAnd :: [Int] -> [Int] -> [Int]
bitwiseAnd xs ys = zipWith bitAnd xs ys

{-
    Ex 20
    
    Write a list comprehension that takes a list of Maybe values and
    returns a list of the Just constructor arguments.
 
    For example,
        [Just 3, Nothing, Just 4] => [3,4]
-}
justVals :: [Maybe a] -> [a]
justVals []             = []
justVals (Nothing :xs)  = justVals xs
justVals ((Just x):xs)  = x : justVals xs

-- provided solution
ex20 :: [Maybe a] -> [a]
ex20 xs = [a | (Just a) <- xs]

{-
    Ex 21
    
    Using a list comprehension, write a function that takes a list of
    Int values and an Int value n and returns those elements in the list 
    that are greater than n.
-}
ex21 :: [Int] -> Int -> [Int]
ex21 xs n = filter (> n) xs

-- provided solution
largerThanN :: [Int] -> Int -> [Int]
largerThanN lst n = [e | e <- lst, e > n]

{-
    Ex 22
    
    Write a function
        f :: [Int] -> Int -> [Int]
    that takes a list of Int values and an Int and returns a list of 
    indexes at which that Int appears.
    
    Note on implementation:
    
    Think this works as the filter is right to left
    ie.  if n==v is True, n gets included, then
         lst !! is applied to n', returning the index of n'
    
-}
-- provided solution
ex22 :: [Int] -> Int -> [Int]
ex22 lst v = [n | n <- [0..length lst - 1], lst!!n == v]

{-
    Ex 23
    
    Write a list comprehension that produces a list giving all of the
    positive integers that are NOT squares in the range 1 to 20.
    
    Note on implementation:
        for every number 'e' between 1 and 20
        compute the square of each number < 'e'
        if the square == 'e', return [e]
        if no square /= 'e', return []
        [] == [] and so 'e' is included in the result list
-}
-- provided solution
ex23 :: [Int]
ex23 = [e | e <- [1..20],
           [x | x <- [1..e], x * x == e] == []]

{-
    Ex 24
    
    Write a function that uses foldr to count the number of times
    a letter occurs in a string.
-}
ex24 :: Char -> String -> Int
ex24 c xs = foldr (count c) 0 xs
    where 
        count :: Char -> Char -> Int -> Int
        count ch x acc = if ch == x then acc + 1 else acc
        
{-
    Ex 25
    
    Write a function using foldr that takes a String and removes each
    instance of a given letter.
-}  
ex25 :: String -> Char -> String
ex25 xs c = foldr (rem c) [] xs
    where
        rem :: Char -> Char -> [Char] -> [Char]
        rem c1 c2 acc = if c1 == c2 
                         then acc else c2 : acc

{-
    Ex 26
    
    Using foldl, write a function
        rev :: [a] -> [a]
    that reverses its list argument.
-}  
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

-- provided solution
rev' :: [a] -> [a]
rev' lst = foldl rearrange [] lst
    where
        rearrange :: [a] -> a -> [a]
        rearrange lst x = x:lst
                       
{-
    Ex 27
    
    Using foldl, write a function
        maybeLast :: [a] -> Maybe a
    that takes a list and returns the last element in it if there is one, 
    otherwise it returns Nothing.
-}          
-- provided solution -- returns Nothing for empty list
maybeLast :: [a] -> Maybe a
maybeLast lst = foldl takeLast Nothing lst
    where
        takeLast :: Maybe a -> a -> Maybe a
        takeLast Nothing x  = Just x
        takeLast (Just y) x = Just x

    

