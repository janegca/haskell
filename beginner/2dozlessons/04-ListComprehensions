-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 4 Computations on Sequences - List Comprehensions

{-  
    Return non-blank characters from a string
    
    Implementation Notes:
        [c | c <- str, c /= ' '] is a 'list comprehension'
        where,
            c - element(s) included in the returned list
            | - read as 'such that'
            c <- str - a 'generator', it runs through every
                       character in the string
            c /= ' ' - the guard (filter); only characters not 
                       equal (/=) to ' ' (predicate is True) are
                       returned in the result list
                       
        i.e. compare each character in the string to the blank character
             ' ' and only return the string character if it is NOT
             equal to the blank character
    
    Example:
        *Main> stripBlank "Madam, I'm Adam"
        "Madam,I'mAdam"    
        *Main> stripBlank ""
        ""
-}    
stripBlank :: String -> String
stripBlank str = [c | c <- str, c /= ' ']

{-
    The stripBlank function can be generalized to filter out
    any character in a string
    
    Example:
        *Main> stripChar '.' "123.344.9"
        "1233449"
        *Main> stripChar '-' "hi-de-hi-di-hi-de-ho"
        "hidehidihideho"
        *Main> stripChar 'o' "toodles"
        "tdles"        
-}
stripChar :: Char -> String -> String
stripChar chr str = [c | c <- str, c /= chr]

{-
    Similar approach when looking to match any character
    
    Example:
        *Main> matchChar '9' "23439.s949"
        "999"
-}
matchChar :: Char -> String -> String
matchChar chr str = [c | c <- str, c == chr]
