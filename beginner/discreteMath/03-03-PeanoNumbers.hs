-- 03.03 Peano Arithmetic
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
{-
    A recursive data structure representing natural numbers.
    
    Example:
        1 = Succ Zero
        3 = Succ (Succ (Succ Zero))
-}
data Peano = Zero 
           | Succ Peano 
    deriving Show

one   = Succ Zero
three = Succ (Succ (Succ Zero))

-- subtracts one form a number by removing a Succ constructor    
-- eg  decrement three -> Succ (Succ Zero)) => 2
decrement :: Peano -> Peano
decrement Zero = Zero
decrement (Succ a) = a

-- adds two Peano numbers by adding recursively adding Succ constructors
-- 
add :: Peano -> Peano -> Peano
add Zero b     = b
add (Succ a) b = Succ (add a b)

{- subtracts two Peano numbers
   negative numbers are not represented so a Zero is returned instead
   eg sub one three -> Zero
      sub three one -> Succ (Succ Zero)
-}   
sub :: Peano -> Peano -> Peano
sub a Zero = a
sub Zero b = Zero
sub (Succ a) (Succ b) = sub a b

-- compare two Peano numbers
--    strips one Succ constructor off at a time, if both numbers reduce to
--    Zero they are equal; if one reduces to zero before the other
--    they are not equal
equals :: Peano -> Peano -> Bool
equals Zero Zero         = True
equals Zero b            = False
equals a Zero            = False
equals (Succ a) (Succ b) = equals a b

-- is one Peano number is less than another
--   strips one Succ constructor off at a time; if the first number
--   reduces to Zero before the other then it is 'less than'
--   the second number
lt :: Peano -> Peano -> Bool
lt a Zero            = False
lt Zero (Succ b)     = True        
lt (Succ a) (Succ b) = lt a b
     
