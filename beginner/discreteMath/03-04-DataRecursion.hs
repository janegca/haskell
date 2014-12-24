-- 03.04 Data Recursion
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

{-
    A technique to define circular data structures.
-}
-- an infinitely long list of 1's as a function
f :: a -> [a]
f x = x : f x

ones = f 1   -- if called, creates an infinite list

-- an infinitely long list as 'data recursion'
-- takes far less memory as continuously points back to itself
twos = 2 : twos     

{-
    Graphs
        A data structure with 'linked' nodes is a 'graph'
        Graphs can be constructed by defining each node with an
        equation in a let expression. This allows each node
        to be referred to by any other node, including itself.
        
        The 'object' below creates an infinite list of numbers
        [1,2,3,1,2,3...]
-}
object = let a = 1 : b
             b = 2 : c
             c = [3] ++ a
         in a
         
         