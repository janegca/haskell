-- 03 Recursion

factorial :: Int -> Int
factorial 0 = 1
factorial (n+1) = (n+1) * factorial n

factorial' :: Int -> Int
factorial' n = product [1..n]

-- return length of a list
{-
    length [1,2,3]
    = 1 + length [2,3]
    = 1 + (1 + length [3])
    = 1 + (1 + (1 + length []))
    = 1 + (1 + (1 + 0))
    = 3    
-}
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

-- sum a list
{-
    sum [1,2,3]
    = 1 + sum [2,3]
    = 1 + (2 + sum [3])
    = 1 + (2 + (3 + sum []))
    = 1 + (2 + (3 + 0))
    = 6
-}
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

{-
    Append in Prelude
    
    (++) :: [a] -> [a] -> [a]
    [] ++ ys = ys
    (x:xs) ++ ys = x : (xs ++ ys)

        [1,2,3] ++ [9,8,7,6]
        = 1 : ([2,3] ++ [9,8,7,6])
        = 1 : (2 : ([3] ++ [9,8,7,6]))
        = 1 : (2 : (3 : ([] ++ [9,8,7,6])))
        = 1 : (2 : (3 : [9,8,7,6]))
        = 1 : (2 : [3,9,8,7,6])
        = 1 : [2,3,9,8,7,6]
        = [1,2,3,9,8,7,6]
-}

{-
    Zip in Prelude
    
    zip :: [a] -> [b] -> [(a,b)]
    zip [] ys = []
    zip xs [] = []
    zip (x:xs) (y:ys) = (x,y) : zip xs ys    

        zip [1,2,3,4] [’A’, ’*’, ’q’]
        = (1,’A’) : zip [2,3,4] [’*’, ’q’]
        = (1,’A’) : ((2,’*’) : zip [3,4] [’q’])
        = (1,’A’) : ((2,’*’) : ((3,’q’) : zip [4] []))
        = (1,’A’) : ((2,’*’) : ((3,’q’) : []))
        = (1,’A’) : ((2,’*’) : [(3,’q’)])
        = (1,’A’) : [(2,’*’), (3,’q’)]
        = [(1,’A’), (2,’*’), (3,’q’)]    
-}

{-
    Concat in Prelude
 
    concat :: [[a]] -> [a]
    concat [] = []
    concat (xs:xss) = xs ++ concat xss
    
        concat [[1], [2,3], [4,5,6]]
        = [1] ++ concat [[2,3], [4,5,6]]
        = [1] ++ ([2,3] ++ concat [[4,5,6]])
        = [1] ++ ([2,3] ++ [4,5,6])
        = [1] ++ [2,3,4,5,6]
        = [1,2,3,4,5,6]   

-}
-- split a list into gt, lt halves and recurse on each half
quicksort :: Ord a => [a] -> [a]
quicksort []            = []
quicksort (splitter:xs) =
    quicksort [y | y <- xs, y<splitter]
    ++ [splitter]
    ++ quicksort [y | y <- xs, y>=splitter]

