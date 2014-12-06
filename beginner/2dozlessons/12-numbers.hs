-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 12 - The Class of Numbers

{-
    The 'Num' class contains 6 subclasses and 8 specific types
    
                 Num                          Integral  Integer
                /   \                                   Int
            Real    Fractional                RealFrac  Rational
             /  \    /       \                          Ratio Int
      Integral   RealFrac   Floating          Floating  Complex Float 
                     \     /                            Complex Double
                    RealFloat     

                          
        
    All subclasses of Num share the operations:
        +, -, *, negate, abs, signum, fromInteger
        
    Other operations are specific to certain classes, for example:
        div, mod, divMod    - Integral
        (/)                 - Fractional
    
-}
-- given a base and the digits of a numeral in reverse order
-- return the value of the number
horner :: Num n => n -> [n] -> n
horner b ds = foldr (multAdd b) 0 ds
    where
        multAdd :: Num n => n -> n -> n -> n
        multAdd b d s = d + b * s
        
-- given a number base and the digits of a number
-- return the value of the number in the given base        
integerFromNumeral :: Num n => n -> [n] -> n
integerFromNumeral b xs = (horner b . reverse) xs

-- given a number base and the value of a number in that base
-- return the individual digits that comprise the number
numeralFromInteger :: (Eq n, Num n, Integral n) => n -> n -> [n]
numeralFromInteger b x 
    = reverse [d | (s,d) <- takeWhile (/= (0,0)) (sdPairs b x)]
    where
        sdPairs :: Integral n => n -> n -> [(n,n)]
        sdPairs b x = iterate (nextDigit b) (x `divMod` b)
        
        nextDigit :: Integral n => n -> (n, a) -> (n, n)
        nextDigit b (xShifted, d) = xShifted `divMod` b
        
{-
    Example Usage:
    
        *Main> integerFromNumeral 10 [1,9,0,0]
        1900
        *Main> integerFromNumeral 2 [1,1,1,0,1,1,0,1,1,0,0]
        1900
        *Main> integerFromNumeral 8 [3,5,5,4]
        1900

        *Main> numeralFromInteger 10 1900
        [1,9,0,0]
        *Main> numeralFromInteger 2 1900
        [1,1,1,0,1,1,0,1,1,0,0]
        *Main> numeralFromInteger 8 1900
        [3,5,5,4]    

-}                
        
