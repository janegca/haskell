-- using Maybe

ex9 :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
ex9 [] []                       = []
ex9 (Nothing:xs) (Nothing:ys)   = Nothing  : ex9 xs ys
ex9 (Nothing:xs) ((Just n):ys)  = (Just n) : ex9 xs ys
ex9 ((Just n):xs)(Nothing:ys)   = (Just n) : ex9 xs ys
ex9 ((Just n):xs) ((Just m):ys) = (Just (n+m)) : ex9 xs ys

-- provided solution
addJust :: Maybe Int -> Maybe Int -> Maybe Int
addJust (Just a) (Just b) = Just (a + b)
addJust (Just a) Nothing  = Just a
addJust Nothing (Just a)  = Just a
addJust Nothing Nothing   = Nothing

addMaybe :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addMaybe lst1 lst2 = zipWith addJust lst1 lst2

-- Ex 10
data Metals = Gold
            | Silver
            | Lead
            | Tin
            | Zinc
            | Copper
    deriving (Eq, Show)
    
-- Ex 11
data Coins n = Pennies n
             | Nickels n
             | Dimes n
             | Quarters n
             | Loonies n
             | Twoonies n
    deriving (Eq, Show)
    
-- provided solution
data Coins' = OneP Int   | TwoP Int     | FiveP Int
            | TenP Int   | TwentyP Int
            | FiftyP Int | HundredP Int
        deriving (Eq, Show)    
        
-- Ex 12
data Utypes = BOOL Bool | CHAR Char | INT Int
        deriving (Eq, Show)
        
-- Ex 13
data Combos a b c d = Pair (a,b) 
                    | Triple (a,b,c)
                    | Quads (a,b,c,d)
        deriving (Eq, Show)
        
-- provided answer        
data Tuples a b c d = Tuple0 | Tuple1 a | Tuple2 a b
                    | Tuple3 a b c | Tuple4 a b c d
    deriving (Eq, Show)        