-- use '@' to assign a 'name' to a pattern
--  allows you to access the whole pattern as well as its parts
--
foo allxs@(x:xs) =    "All x's are: " ++ (show allxs) ++ "\n"
                   ++ "The first x is: " ++ (show x) ++ "\n"
                   ++ "The rest of the x's are: " ++ (show xs)

{- usage (ghci):
    *Main> putStrLn $ foo [1,2,3,4]
    All x's are: [1,2,3,4]
    The first x is: 1
    The rest of the x's are: [2,3,4]
-}


