-- Hutton Fold Tutorial - Ackerman

{-
    As a last example of the power of fold, consider the Ackerman 
    function process two lists of integer and is defined by
    explicit recursion as
-}

ack :: [Int] -> ([Int] -> [Int])
ack [] ys             = 1 : ys
ack (x:xs) []         = ack xs [1]
ack (x : xs) (y : ys) = ack xs (ack (x : xs) ys)

{-
    To redefine this in terms of fold
    
        ack []     = v
        ack (x:xs) = f x (ack xs)
        
    Where we can see v = (1:) but the second equation has to be
    redefined to give us ack (x:xs) = fold g w, which implies
    
        ack (x:xs) [] = w
        ack (x:xs) (y:ys) = g y (ack (x:xs) ys)
    
    Now the first equation gives us w = ack xs [1] and the
    second equaltion is used to calculate g as:
    
        ack (x:xs) (y:ys) = g y (ack (x:xs) ys)
    ==> ack xs (ack (x : xs) ys) = g y (ack (x : xs) ys)        def ack
    ==> ack xs zs = g y zs                                      generalize
    ==> g = /y -> ack xs                                         functions
    
    So, 
        ack (x : xs) = fold (\y -> ack xs) (ack xs [1])
        
    And from this we can calculate f as:
    
        ack (x : xs) = f x (ack xs)
    ==> fold (\y -> ack xs) (ack xs [1]) = f x (ack xs)
    ==> fold (\y -> g) (g [1]) = f x g
    ==> f = \x g -> fold (\y -> g) (g [1])
    
    Thus, we used the universal property twice to get
    
        ack = fold (\x g -> fold (\y -> g) (g [1])) (1 :)
-}        
        
        
        