-- Informatics 1 - Functional Programming 
-- Lecture 10 - Maybe, Maybe Not
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect10.pdf
-- Video: 3/11/2014
--   http://groups.inf.ed.ac.uk/vision/VIDEO/2014/inf1fp.htm

{-
    MAYBE
        optional data type
        data may be Nothing or Just
        
        defined as:
        
            data Maybe a = Nothing | Just a
-}

-- optional argument
power :: Maybe Int -> Int -> Int
power Nothing n  = 2 ^ n    -- default, assume we want a power of 2
power (Just m) n = m ^ n

-- optional result
divide :: Int -> Int -> Maybe Int
divide n 0 = Nothing
divide n m = Just (n `div` m)

-- using an optional result
{-
wrong :: Int -> Int -> Int
wrong n m = divide n m + 3      -- compile error, fails type checks
-}

-- case analysis on the result of divide
-- cases have to use patterns
right :: Int -> Int -> Int
right n m = case divide n m of
                Nothing -> 3
                Just r  -> r + 3
                
{-
    EITHER/OR
        union of two types
        lets you create lists with two types
        acts as a 'decorator'
        allows a limited form of type checking
        
        Defined as:
            data Either a b = Left a | Right b
-}                
-- values in the list are 'tagged' to indicate data type
mylist :: [Either Int String]
mylist = [Left 4, Left 1, Right "hello", Left 2,
          Right " ", Right "world", Left 17]

addints :: [Either Int String] -> Int
addints []             = 0
addints (Left n : xs)  = n + addints xs
addints (Right s : xs) = addints xs

addints' :: [Either Int String] -> Int
addints' xs = sum [n | Left n <- xs]        

addstrs :: [Either Int String] -> String
addstrs [] = ""
addstrs (Left n : xs) = addstrs xs
addstrs (Right s : xs) = s ++ addstrs xs

addstrs' :: [Either Int String] -> String
addstrs' xs = concat [s | Right s <- xs]
        
{-
    SUBLISTS OF LISTS
        all possible sublist combinations
-}        

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ [ x:ys | ys <- subs xs ]

{-
    *Main> subs "b"
    ["","b"]
    *Main> subs ["a","b"]
    [[],["b"],["a"],["a","b"]]
    subs ["a","b","c"]
    [[],["c"],["b"],["b","c"],["a"],["a","c"],["a","b"],["a","b","c"]]

-}
