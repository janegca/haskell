-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 21 - Patterns as Formal Parameters

{-
    SEQUENCE CONSTRUCTOR (:) - also called 'cons'
    inserts a new element at the beginning of an existing sequence
        x : xs = [x] ++ xs
        where, 
            x   is a new element
            xs  is the existing sequence
            
    The sequence constructor can be used in pattern matching when
    defining functions
-}
firstAndLast :: [String] -> String
firstAndLast (xs : yss) = [head xs] ++ [last(last(xs:yss))]
firstAndLast []         = []

{-
    The 'firstAndLast' function has 2 cases, the first matches
    on a sequence [String] that has at least one or more elements
    If that is not the case, it matches on the empty sequence
    
    Example:
    
        *Main> firstAndLast ["A","Few","Words"]
        "As"
        *Main> firstAndLast ["Only","Two"]
        "Oo"
        *Main> firstAndLast ["one"]
        "oe"
        *Main> firstAndLast []
        ""    
        
    The function could have been written using guards
-}
firstAndLast' :: [String] -> String
firstAndLast' xss
    | null xss  = []
    | otherwise = [head(head xss)] ++ [last(last xss)]
    
{-
    But the first method has the advantage of allowing us to
    attach names to components of the sequence that can be used
    in the formula on the right hand side of the equation
    
    Other example:
        f(x:xs) = g x xs  is equivalent to 
        f xs | not(null xs) = g (head xs) (tail xs)
        
        
        
-}
