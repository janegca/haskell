module TFWH05E where

import Data.List (sort, nub)     -- for Exercise D

{-
    Chapter 5 - A Simple Sudoku Solver
    
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/

-}
{-
    Notes:
    
    Sudoku is played on a 9x9 grid.
    To solve, fill in the grid so every row, every column, and
    every 3x3 grid contains the numbers 1 through 9
    No math is involved, only reasoning and logic.
    
    The approach taken below is known as 'wholemeal programming'
    the Sudoku grid, in the functions, is treated as a complete entity,
    a Matrix, rather than as a set of individual elements. This means
    that:
        rows . rows = id
        cols . cols = id
        boxs . boxs = id   -- if the group size is compatible
        ungroup . group   = id
        group   . ungroup = id
        
        map rows . expand = expand . rows
        map cols . expand = expand . cols
        map boxs . expand = expand . boxs
        
        map (map f)    . cp = cp . map (map f)
        filter (all p) . cp = cp . map (filter p)

-}
type Matrix a = [Row a]
type Row a    = [a]
type Grid     = Matrix Digit
type Digit    = Char
type Choices  = [Digit]

digits :: [Char]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = ( == '0')

solve1 :: Grid -> [Grid]
-- given a grid, fill in every possible choice for the blanks
-- returning a list filled grids, filter the list for those
-- grids that do not contain duplicates in any row, box or column
solve1 = filter valid . expand . choices 

choices :: Grid -> Matrix [Digit]
-- put available digits in blank cells
choices = map (map choice)
    where
        choice d = if blank d then digits else [d]

cp :: [[a]] -> [[a]]
-- compute the Cartesian product (all possible combinations of a
-- list of lists)
cp [] = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- yss]
    where yss = cp xss

expand :: Matrix [Digit] -> [Grid]
-- compute all possible row combinations in a matrix
-- returns an empty list if any cell in the matrix is blank
expand = cp . map cp

valid :: Grid -> Bool
-- a valid if no row, column or grid contains duplicate entries
valid g = all nodups (rows g)
       && all nodups (cols g)
       && all nodups (boxs g)
       
nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = all (/= x) xs && nodups xs
       
rows :: Matrix a -> Matrix a
-- if a matrix is a list of rows then rows is simply the identity
-- function for matrices
rows = id       

cols :: Matrix a -> Matrix a
-- cols is simply a transform of a matrix
cols [xs]     = [ [x] | x <- xs ]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
-- 
boxs = map ungroup . ungroup 
     . map cols . group 
     . map group
     
group :: [a] -> [[a]]
-- split a list into groups of three
group [] = []
group xs = take 3 xs:group (drop 3 xs)

ungroup :: [[a]] -> [a]
-- recombines a grouped list
ungroup = concat

{-
    2. Pruning Choices
    
    Our first solve involves 9^61 possibilities; way to many
    to be practical.
    
    The first attempt at pruning our choices down is to 
    remove any choice that is duplicate a row, column or box 
    and re-writing 'solve' to use the prune function.
-}     
solve2 :: Grid -> [Grid]
solve2 = filter valid . many prune . choices

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
    where pruneBy f = f . map pruneRow . f
        
pruneRow :: Row Choices -> Row Choices
-- remove all elements of a row that are not singletons
pruneRow row = map (remove fixed) row
    where fixed = [ d | [d] <- row ]

remove :: Choices -> Choices -> Choices
-- remove any element that is already in the list
remove ds [x] = [x]
remove ds xs  = filter (`notElem` ds) xs

-- using the original example in the text produces an error
-- as actual digits are used when Digit has been defined as a Char
-- when switching them to Char, the concat combines individual
-- digits into full numbers
ex1 = pruneRow [['6'],['1','2'],['3'],['1','3','4'],['5','6']]
    -- ["6","12","3","14","5"]
    
many :: Eq a => (a -> a) -> a -> a
-- returns a matrix of choices which can be:
-- 1. a complete matrix filled with singleton choices
-- 2. a matrix that contains an empty choice
-- 3. a matrix with duplicate choices
many f x = if x == y then x else f y
    where y = f x
    
{- 3. Single Cell Expansion 
    
    We don't want to do a full expansion on a matrix that has
    duplicate choices; so we create a partial function to handle
    pruning for the duplicates only
-}       

expand1   :: Matrix Choices -> [Matrix Choices]
-- expands rows with duplicate choices
expand1 rows =
    [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
    where
        -- break the matrix down into two lists of rows
        -- the row at the head of the second list contains
        -- a non-singleton choice; that row is then broken
        -- into two lists with the head of the second list
        -- being the first non-singleton element
        (rows1,row:rows2) = break (any smallest) rows
        (row1,cs:row2)    = break smallest row
        smallest cs       = length cs == n
        n                 = minimum (counts rows)
        
        counts = filter (/= 1) . map length . concat
        
complete :: Matrix Choices -> Bool
-- a Matrix is complete boxes, rows and columns contain singleton entries
complete = all (all single)   
    where single [_] = True
          singel  _  = False

safe :: Matrix Choices -> Bool
-- a matrix is safe if none of the singleton choices in any row,
-- column or box is a duplicate; although the matrix as a while
-- may contain duplicate choices
safe cm = all ok (rows cm)
       && all ok (cols cm)
       && all ok (boxs cm)
    where ok row = nodups [ x | [x] <- row]
    
-- final solve
solve :: Grid -> [Grid]
solve = search . choices    

search :: Matrix Choices -> [Grid]
search cm
    |not (safe pm)  = []
    |complete pm    = [map (map head) pm]
    |otherwise      = (concat . map search . expand1) pm
    where pm = prune cm

-- ---------------------------------------------------------------------
-- Exercises
-- ---------------------------------------------------------------------

-- Exercise A
-- A1. How would you add 1 to every element of a given matrix of integers?

mat1, mat2 :: Matrix Int
mat1 = [[1,2,3],[4,5,6],[7,8,9]]
mat2 = [[9,8,7],[6,5,4],[3,2,1]]

addOne :: Num a => Matrix a -> Matrix a
addOne = map (map (+1))

exA1 = addOne mat1

-- A2. How would you sum the elements of a matrix?
sumMat :: Num a => Matrix a -> a
sumMat m = sum $ map sum m

exA2 = sumMat mat1

-- A3. The function zipWith (+) adds two rows but how would you add
--     two matrices?
addMats :: Num a => Matrix a -> Matrix a -> Matrix a
addMats (x:xs) (y:ys) = zipWith (+) x y : addMats xs ys
addMats  _      _     = []

exA3 = addMats mat1 mat2

-- A4. How would you define matrix multiplication?
--
scaleMat :: (Num a) => a -> Matrix a -> Matrix a
-- multiply a matrix by a scalar
scaleMat s m = map (map (*s)) m

exA4 = scaleMat 2 exA3

-- provided solutions
scalarMult :: Num a => [a] -> [a] -> a
-- multiply two rows
scalarMult xs ys = sum (zipWith (*) xs ys)

exA4a = scalarMult [1,2,3] [4,5,6]

matMult :: Num a => Matrix a -> Matrix a -> Matrix a
matMult ma mb = [ map (scalarMult row) mbt | row <- ma ]
    where mbt = cols mb
    
exA4b = matMult mat1 mat2

-- Exercise B
-- B1. What are the dimensions of [[],[]]? []?
--     Ans: 2x1, 1x1
--     Provided Answer: 2x0, 0 x n

-- B2. Rewrite 'cols' above as a transpose that proceeds column
--     by column vs row by row

-- provided solution
transpose :: Matrix a -> Matrix a
transpose []      = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

exB2 = transpose mat1 == [[1,4,7],[2,5,8],[3,6,9]]

-- Exercise C
-- Which of the following equations are true?
--      any p    = not . all (not p)        -- True
--      any null = null . cp                -- True

-- Exercise D
--      Given a function: sort :: Ord a => [a] -> [a],
--      write a function: nodups :: Ord a => [a] -> Bool
--      [Note: to do the same over matrices would need to use
--             nub . (map nub)
nodups' :: Ord a => [a] -> Bool
nodups' lst = nub xs == xs
    where  xs = sort lst
    
-- provided solution
nodups'' :: Ord a => [a] -> Bool
nodups'' xs = and (zipWith (/=) ys (tail ys))
    where ys = sort xs
    
-- Exercise E
--      Define nub  (the last duplicated element is kept)     
nub' :: (Ord a) => [a] -> [a]
nub' (x:xs) | elem x xs = nub' xs
            | otherwise = x : nub' xs      
nub' _ = [] 

exE = nub' [1,1,2,4,2]

-- provided solutions 
-- Note: the first duplicated is kept
nub1 :: (Ord a) => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x : nub (filter (/= x) xs)

nub2 :: (Ord a) => [a] -> [a]
nub2 = remdups . sort
    where remdups [] = []
          remdups (x:xs) = x : remdups (dropWhile (== x) xs)

-- Exercise F
-- F1. Give direct recursive definitions of takeWhile and dropWhile

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x:xs) | f x       = x : takeWhile' f xs
                    | otherwise = takeWhile'  f xs
takeWhile' _ [] = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f xs@(x:xs') | f x       = dropWhile' f xs'    
                        | otherwise = xs
dropWhile' _ [] = []

lst = [2,4,6,5,3,1]
exF1a = takeWhile' even lst == takeWhile even lst
exF1b = dropWhile' even lst == dropWhile even lst

-- provided solutions
takeWhile1, dropWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p []     = []
takeWhile1 p (x:xs) = if p x then x : takeWhile1 p xs else []

dropWhile1 p [] = []
dropWhile1 p (x:xs) = if p x then dropWhile1 p xs else x:xs

-- F2. Assuming whiteSpace :: Char -> Bool is a test for whether a
--     character is white space or visible, construct a definition
--     words :: String -> [Word] that breaks a string into a list
--     of words
type Word = String

-- based on provided solution
words' :: String -> [Word]
words' xs  | null ys = []
           | otherwise = w : words' zs
    where whiteSpace x = elem x [' ', '\n', '\t']
          ys           = dropWhile' whiteSpace xs
          (w,zs)       = break whiteSpace ys
         
exF2 = words' "If we don't manage the present there will be no future \
              \ -- James Baldwin"               
              
-- Exercise G
--      Define:  minimum
minimum' :: Ord a => [a] -> a
minimum' []  = error "empty list"
minimum' lst = f (head lst) lst
    where f m (x:xs) | x < m     = f x xs
                     | otherwise = f m xs
          f m [] = m
          
-- provided solution (Note that empty list is undefined)
minimum'' :: Ord a => [a] -> a
minimum'' [x] = x
minimum'' (x:xs) = x `min` minimum xs     

   
                     