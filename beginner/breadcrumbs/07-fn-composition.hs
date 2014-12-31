{-
    Function composition 
        use when you have a sequence of functions acting
        on data; like pipes or connecting streams in Java
        
        The 'data' to be acted on is implicit (Haskell uses
        the function type to determine what arguments are
        expected, allowing us to omit some when writing our code.)
-}
import Data.List (sort)

-- the the minimum 'n' values in a sorted list of values
min_n n = (take n) . sort
                                        
min_n' n lst = ((take n) . sort) lst   -- same as min_n

-- use the function application operator ($) to remove parentheses
min_n'' n lst = (take n) . sort $ lst                                      

-- sort and reverse a list
descending_sort lst = (reverse . sort) lst

-- we cannot omit the 'lst' argument for this function
--
-- In the 'min_n' example, (take n) DOES NOT need to know the
-- type of the elements in the list; that is not true for
-- 'reverse'
--
-- the following definitions produce type ambiguity errors
--
-- descending_sort' = reverse . sort
-- descending_sort' = (reverse . sort)

{-
    Example output:

    *Main> min_n 5 ['a'..'z']
    "abcde"
    *Main> min_n' 5 ['a'..'z']
    "abcde"
    *Main> min_n'' 5 ['a'..'z']
    "abcde"

    *Main> descending_sort ['A'..'Z']
    "ZYXWVUTSRQPONMLKJIHGFEDCBA"

-}