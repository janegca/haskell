
{-
    Exercise: Validating a Credit Card Number
    
    - double the value of every second digit from the right
      i.e. [1,3,8,6] => [2,3,16,6]
      
    - add the 'digits' from the above result (double digit
      numbers need to be split into single digits]
      i.e. [2,3,16,6] => 2 + 3 + 1 + 6 + 6 = 18
      
    - calculate the remainder when the sum is divided by 10
      if the result is 0 the number is valid
      i.e. 18 / 10 = 8 not equal to 0 therefore the original number
                       (1386) is not a valid credit card number
-}

-- get the last digit of a decimal number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- drop the last digit from a decimal number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- convert a positive integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
    | n < 0     = []
    | otherwise = (toDigits (dropLastDigit n)) ++ [lastDigit n]
  
-- double every other digit in a list of digits beginning from the right  
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther  xs = doubley (reverse xs)
    where
        doubley :: [Integer] -> [Integer]
        doubley []       = []
        doubley [x]      = [x]
        doubley (x:y:xs) = doubley xs ++ 2*y : x : []
   
-- sum the individual digits for all integers in a list of 
-- positive integers  
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs
       
-- validate a given credit card number       
validate :: Integer -> Bool
validate n
    = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0    
    
    
