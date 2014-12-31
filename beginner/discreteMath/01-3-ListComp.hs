-- List comprehension exercises
{
    [ returned element | from the given list, if predicate is True]
}
-- no list elements in result as predicate is always False
ex2a = [x | x <- [1,2,3], False]    -- []

{- negates logical and 
       x      y                      
    [False, False] -> not (False && False) -> not (False) -> True
    [False, True]  -> not (False && True ) -> not (False) -> True
    [True, False]  -> not (True && False ) -> not (False) -> True
    [True, True]   -> not (True && True  ) -> not (True)  -> False
-}
ex2b = [not (x && y) | x <- [False,True],   -- outer loop
                       y <- [False,True]]   -- inner loop

ex2c = [x || y | x <- [False,True],
                 y <- [False,True],
                 x /= y]                -- [True, True]

{- equivalent to nesting three levels deep         

    Example:
    
    *Main> [(x,y,z) | x<-[1..2], y<-[3..4],z<-[5..6]]
    [(1,3,5),(1,3,6),
     (1,4,5),(1,4,6),
     (2,3,5),(2,3,6),
     (2,4,5),(2,4,6)]
-}        
ex2d = [[x,y,z] | x <- [1..150],
                  y <- [1..150],
                  z <- [1..150],
                  x**2 + y**2 == z**2]