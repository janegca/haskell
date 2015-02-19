-- Function composition 

{- References:
    [TDSL]: Two Dozen Short Lessons in Haskell by Rex Page
-}

{-
    [TDSL] - function composition with 'foldl'
    
    In the formula:  foldr1 f [a,b,c,d]
        - a, b, c and d must have the same type
        - f must deliver a result of the same type as its arguments
        - f must be a function that takes two arguments

-}

-- removeBPC, foldRemove and mapRemove are equivalent [TDSL]
remove char str = [c | c <- str, c /= char]
removeBPC   str = (remove ',' . remove ' ' . remove '.') str

foldRemove  str = foldr1 (.) [remove ',', remove ' ', remove '.'] str
mapRemove str   = foldr1 (.) [ remove c | c <- ",. "] str