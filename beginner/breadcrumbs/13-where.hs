-- using 'where' to limit scope of helper functions
-- anything declared in the 'where' clause is visible only to
-- the function under which it is defined

import Data.List (sort)

max_list lst = head rslst 
    where                  -- defn's only visible to max_list fn
        rslst = reverse slst
        slst  = sort lst

chocolate pie = delicious pie 
    where                   -- defn'sonly visible to 'chocolate' fn
        delicious = (++) "your mom's "  
    
{-
    Example:
         Note that alpha characters are sorted by their ordinal
         numbers; capital letters run from 65 to 80 while
         lower case letters run from 97 to 122
    
    *Main> max_list $ ['a'..'z'] ++ ['A'..'Z']
    'z'
    
    *Main> chocolate "apple"
    "your mom's apple"
-}    