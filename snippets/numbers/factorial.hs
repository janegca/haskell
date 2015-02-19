-- Factorial routines

{-
    References:
        [DM]    Discrete Math using a Computer
        
-}

-- routines from [DM]
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Int -> Int
factorial' n = product [1..n]

