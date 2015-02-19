-- quadratic function
import Data.Maybe 

-- Reference: Discrete Math using a Computer

-- solves the expression: x * x^2 + b * x + c = 0 with two real solutions
-- (x1,x2)

quadratic :: Double -> Double -> Double -> (Double,Double)
quadratic a b c
    = let d = sqrt (b^2 - 4*a*c)
          x1 = (-b + d) / (2*a)
          x2 = (-b - d) / (2*a)
      in (x1,x2)
      
{-
    Example Usage
    
        *Main> quadratic' 2 5 (-3)
        Just (0.5,-3.0)
        *Main> quadratic' 2 4 (-4)
        Just (0.7320508075688772,-2.732050807568877)
        *Main> quadratic' 0 4 (-4)
        Nothing    
-}      
      
quadratic' :: Double -> Double -> Double -> Maybe (Double,Double)
quadratic' a b c =
    if a == 0 then Nothing
    else let d = sqrt (b^2 - 4*a*c)
             x1 = (-b + d) / (2*a)
             x2 = (-b - d) / (2*a)
         in (Just (x1,x2))
      