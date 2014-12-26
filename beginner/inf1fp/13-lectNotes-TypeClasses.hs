-- Informatics 1 - Functional Programming 
-- Lecture 13-14 - Type Classes
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  10/11/2014  33:00 minute mark
--
-- TODO - need to supplement this whole bit; find other sources
--
{-
    Declaration for the 'elem' function
    
        elem :: Eq a => a -> [a] -> Bool
        
    'Eq a =>' is a CONSTRAINT on the types of data that can be used
    by the formula
    
    The function itself HAS TO COMPARE ELEMENT, regardless of how 
    it is implemented
    
        -- comprehension
        elem x ys = or [ x == y | y <- ys ]

        -- recursion
        elem x [] = False
        elem x (y:ys) = x == y || elem x ys

        -- higher-order
        elem x ys = foldr (||) False (map (x ==) ys)

    So the data MUST be comparable; and this is what the 'Eq a =>'
    constraint declares.
-}
{-
    Definition of Equality Type Class - creates 'instances' of various
    types, defining 'how' members of the type are 'equal'
    
        class Eq a where
        (==) :: a -> a -> Bool
        
        instance Eq Int where
        (==) = eqInt                -- eqInt is a machine instruction
        
        instance Eq Char where
        x == y = ord x == ord y
        
        instance (Eq a, Eq b) => Eq (a,b) where
        (u,v) == (x,y) = (u == x) && (v == y)
        
        instance Eq a => Eq [a] where
        [] == []     = True
        [] == y:ys   = False
        x:xs == []   = False
        x:xs == y:ys = (x == y) && (xs == ys)    

-}
