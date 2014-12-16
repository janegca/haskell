-- Tutorial 3 High-order Functions - Optional Assignment
-- functions given in the provided solution
-- Note: these are not efficient for handling large matrices

import Test.QuickCheck
import Data.List (transpose)
import Data.Ratio

type Matrix a = [[a]]

--
-- Finding the determinant
--
-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]        
removes []     = []
removes (x:xs) = xs : map (x :) (removes xs)

-- Produce a matrix of minors from a given matrix
minors :: Matrix a -> Matrix (Matrix a)
minors m = map (map transpose . removes . transpose) (removes m)

zipMatrix :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipMatrix f = zipWith (zipWith f)

determinant :: Num a => Matrix a -> a
determinant [[x]] = x
determinant m = sum $ zipWith (*) row (cycle [1,-1])
  where f x m = x * determinant m
        row   = head $ zipMatrix f m (minors m)
        
--
-- Finding the Inverse
--
timesM :: Num a => Matrix a -> Matrix a -> Matrix a
timesM m1 m2 | ok        = [ [ dot row col | col <- transpose m2 ] 
                                | row <- m1 ]
              | otherwise = error "Invalid input matrices."
  where dot xs ys = sum (zipWith (*) xs ys)
        ok        = matrixWidth m1 == matrixHeight m2

matrixWidth :: Matrix a -> Int
matrixWidth m = length (head m)

matrixHeight :: Matrix a -> Int
matrixHeight m = length m

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix f = map (map f)

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Num a => Int -> Int -> Matrix a
signMatrix w h = cycleN h [evenRow, oddRow]
  where evenRow     = cycleN w [1,-1]
        oddRow      = cycleN w [-1,1]
        cycleN n xs = take n (cycle xs)
        
cofactors :: Num a => Matrix a -> Matrix a
cofactors m = zipMatrix (*) (mapMatrix determinant $ minors m) signs
  where signs = signMatrix (matrixWidth m) (matrixHeight m)

scaleMatrix :: Num a => a -> Matrix a -> Matrix a
scaleMatrix k = mapMatrix (k *)

inverse :: Fractional a => Matrix a -> Matrix a
inverse m = scaleMatrix (1 / determinant m) (transpose $ cofactors m)     

-- Tests
identity :: Num a => Int -> Matrix a
identity n = map f [0..n - 1]
  where f m = take n $ replicate m 0 ++ [1] ++ repeat 0

prop_inverse2 :: Ratio Integer -> Ratio Integer -> Ratio Integer 
                -> Ratio Integer -> Property
prop_inverse2 a b c d = determinant m /= 0 ==> 
                       m `timesM` inverse m    == identity 2
                       && inverse m `timesM` m == identity 2
  where m = [[a,b],[c,d]]
        
type Triple a = (a,a,a)
        
prop_inverse3 :: Triple (Ratio Integer) -> 
                 Triple (Ratio Integer) -> 
                 Triple (Ratio Integer) ->
                 Property
prop_inverse3 r1 r2 r3 = determinant m /= 0 ==> 
                         m `timesM` inverse m    == identity 3
                         && inverse m `timesM` m == identity 3
  where m           = [row r1, row r2, row r3]
        row (a,b,c) = [a,b,c] 

-- test matrices for checking determinants
m1 = [[2]]                  -- determinant s/b 2

m2 = [ [1, 0,  1, 1],       -- determinant s/b 4
       [2, 2, -1, 1],
       [2, 1,  3, 0],
       [1, 1,  0, 1]]
  
m3 :: Matrix Integer  
m3 = [ [1,  0,  1, 1],      -- determinant s/b 0
       [2, -1, -1, 1],
       [2,  5,  3, 0],
       [1, -1,  0, 1]]
       
m4 = [ [1,0,1],
       [1,1,1],
       [2,-1,1]]
       
