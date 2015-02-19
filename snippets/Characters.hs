-- Char operations
module Characters where

{- References:
    [TDSL]: Two Dozen Short Lessons in Haskell by Rex Page
-}

import Data.Char (isUpper)

-- translate a Char to its ASCII value [TDSL]
intFromLtr :: Char -> Int
intFromLtr c 
    | isUpper c  = fromEnum c - fromEnum 'A'
    | otherwise  = error "not uppercase"

-- translate an ASCII value to a Char [TDSL]
ltrFromInt :: Int -> Char
ltrFromInt n
    | n >= fromEnum 'A' = toEnum(n + fromEnum 'A')
    | otherwise         = error "not uppercase"
