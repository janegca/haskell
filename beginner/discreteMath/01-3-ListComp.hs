-- List comprehension exercises

ex2a = [x | x <- [1,2,3], False]    -- []

ex2b = [not (x && y) | x <- [False,True],
                       y <- [False,True]]   
-- [True, True, True, False]             

ex2c = [x || y | x <- [False,True],
                 y <- [False,True],
                 x /= y]                -- [True, True]

ex2d = [[x,y,z] | x <- [1..150],
                  y <- [1..150],
                  z <- [1..150],
                  x**2 + y**2 == z**2]