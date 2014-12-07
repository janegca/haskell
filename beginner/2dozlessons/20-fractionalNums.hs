-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 20 - Fractional Numbers

import Data.Ratio

{-
    FLOATING POINT NUMBER REPRESENTATION
    
    Float and Double numbers have two parts: mantissa and exponent
    The mantissa can be viewed as a whole number with a fixed number
    of digits.
    
    The exponent can be viewed as another whole number that specifies
    a scaling factor for the mantissa. The scaling factor will be
    the power of the radix of the number system being used. In effect,
    the exponent moves the decimal point to the right (if exponent
    is positive) or left (if the exponent is negative) within the mantissa.
    
    Example:
        
        mantissa  exp
        
        1.89533 x 10^25     -> 1.89533e25
        1.05522 x 10^(-24)  -> 1.05522e-24

    Numbers that are Doubles carry twice the precision of Floats
    ie. their mantissa has more digits
-}
{-
    display an analog (fractional) value on a digital sale
    
    i.e. x will have a value somewhere between a and b on available
         digital levels {0,1,2...n}
         we want to divide the range a to b into n-segments labelled
         {0,1,2,...n} and figure out which segment 'x' falls into
         the label of that segment will be the digital value of 'x'
         
    So, n   = number of digital levels {0,1,2,...n-1}
        a   = lower bound of range
        b   = higher bound of range
        x   = value 
        dx  = step-size within range where
                dx = (b - a) / n
                
        digital level = floor( (x - a) / dx )
        
    Example:
    
        *Main> digitize 8 0.3 1.9 0.84
        2                             
        
        *Main> putStr(showGraph 40 sin(-2*pi)(2*pi))
          ******              ******
        **      **          **      **
                  **      **          **      **
                    ******              ******        
        
-}
-- n-way analog-to-digital converter for a <= x < b
digitize:: RealFrac num => Int -> num -> num -> num -> Int
digitize n a b x
    | xDist < halfStep                     = 0
    | xDist > (analogRangeSize - halfStep) = nSafe - 1
    | otherwise                            = floor( xDist / dx )
    where
        xDist = x - a
        dx = analogRangeSize / (fromIntegral nSafe)
        halfStep = dx/2
        
        nSafe | n > 0 = n
              | otherwise = error "digitize: zero or negative levels"
              
        analogRangeSize = b - a

-- spaces, reps and transpose were pulled from utility files        
spaces :: Int -> String        
spaces numberOfSpaces = reps numberOfSpaces ' '

reps :: Int -> a -> [a]
reps n = take n . repeat

transpose :: [[a]] -> [[a]]
transpose = foldr patchRowAcrossColumns [ ]
    where
        patchRowAcrossColumns row columns =
            zipWith (:) row (columns ++ repeat [ ])
  
showGraph:: RealFrac num => Int -> (num->num) -> num -> num -> String
showGraph w f a b = (unlines . reverse . transpose) graph
    where
        graph = [spaces y ++ "*" ++
                 spaces(height - 1 - y)| y <- ysDigitized]
        ysDigitized = [digitize height yMin yMax y | y<-ys]
        height = max 1 (round(fromIntegral(w)*
                        aspect * (yMax - yMin)/(b - a)))
            
        ys = [f x| x<-xs] -- ordinates
        
        -- centred abscissas (you define xs)
        xs = [a + dx/2 + fromIntegral(k)*dx| k<-[0..w-1]]
        
        -- step size for abscissa (you define dx)
        dx = (b-a)/fromIntegral(w)
        
        yMax   = maximum ys
        yMin   = minimum ys        
        aspect = fromRational( 3 % 5)
        
        
        
        
