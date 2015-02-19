-- digits

-- Source: http://www.seas.upenn.edu/~cis194/lectures/01-intro.html

-- return the last digit in a decimal number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- drop the last digit in a decimal number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- given a decimal number as a String, split into whole and fractional
-- parts [DM]
numParts :: String -> (String, String)
numParts str = (wholeNum str, fracNum str)
    where
        wholeNum :: String -> String
        wholeNum []     = ""
        wholeNum (x:xs) | x == '.' = []
                        | otherwise = x : wholeNum xs
                       
        fracNum :: String -> String
        fracNum [] = ""
        fracNum (x:xs) | x == '.'  = xs
                       | otherwise = fracNum xs

-- return a decimal number as a list of digits
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
    | n < 0     = []
    | otherwise = (toDigits (dropLastDigit n)) ++ [lastDigit n]

-- sum a list of digits (breaks larger numbers into single digits)
-- ie sumDigits [16,7,12,5] == 1+6+7+1+2+5 == 22
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs
