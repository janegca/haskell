{-
    CIS 552: Advanced Programming (2015)
    
    Homework 1
    
    Provides practise with the basic built-in data structures of Haskell, 
    including lists, tuples and maybes, as well as recursion and pattern
    matching. It also covers the basics of code style and test-driven 
    development.
    
    Source:
    https://www.seas.upenn.edu/~cis552/current/hw/hw01/index.html
    
    Ref for Problem 1 Bowling Kata:
        https://en.wikipedia.org/wiki/Ten-pin_bowling#Scoring

-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
-- add WError to the options when all finished, will turn
-- 'warnings' into 'errors'
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude hiding (all, reverse, takeWhile, zip)
import Test.HUnit 
import Control.Applicative ((<*>))

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ test0, test1, test2, test3, test4 ]
   return ()

-- Part 0 (a)
--    Rewrite the following so they follow the style guide

abc :: Bool -> Bool -> Bool -> Bool
-- | True if x is True or either of y or z is True, otherwise, False
abc x y z | x         = if y then y else z
          | otherwise = False
 
t0a :: Test
t0a = "0a1" ~: TestList [abc True  False True  ~?= True, 
                         abc True  False False ~?= False,
                         abc False True  True  ~?= False]

-- 0 (b)
arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
-- | Arithmetic exercise
arithmetic ((a,b),c) ((d,e),f) =
    (b*f - c*e, c*d - a*f, a*e - b*d)
 
t0b :: Test
t0b = "0b" ~: TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3), 
                        arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

-- 0 (c)
-- | Maximum list element or a given integer
cmax :: [Int] -> Int -> Int
cmax l t = max (maximum l) t

t0c :: Test
t0c ="0c" ~: TestList[ cmax [1,4,2] 0 ~?= 4, 
                       cmax []      0 ~?= 0,
                       cmax [5,1,5] 0 ~?= 5 ]

-- 0 (d)
reverse :: [a] -> [a]
-- | Reverse the elements in a list.
reverse = foldl (\acc x -> x : acc) []

t0d :: Test
t0d = "0d" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                        reverse [1]     ~?= [1] ]

test0 :: Test
test0 = "test0" ~: TestList [ t0a , t0b, t0c, t0d ]


-- Problem 1
--  The goal of this problem is to write a function that calculates the
--  score of a bowling game. The point of the kata (exercise) is to
--  use test cases to drive the development of the score function; it's
--  a design exercise (TDD - test driven development). Begins with
--  writing simple tests cases, getting them to pass, writing more
--  complicated test cases, getting them to pass, etc.
--
--  Notes on 10-pin Bowling:
--      A game is 10 frames
--      Each player is allowed to throw two balls per frame
--
bowlingTest0 :: ([ Int ] -> Int) -> Test
bowlingTest0 score = "gutter ball" ~: 0 ~=? score (replicate 20 0)

score0 :: [ Int ] -> Int
score0 _ = 0

step0 :: IO Counts
step0 = runTestTT (bowlingTest0 score0)     -- passes

bowlingTest1 :: ([ Int ] -> Int) -> Test
bowlingTest1 score = 
   "allOnes" ~: 20 ~=? score (replicate 20 1)

score1 :: [ Int ] -> Int
score1 = score where
   score = sum
   
-- test both cases using new scoring function  
step1 :: IO Counts 
step1 = runTestTT (TestList [(bowlingTest0 score1),
                             (bowlingTest1 score1) ])   -- passes

-- Scoring for a spare
--      A spare happens when all pins are knocked down in one frame
--      The frame gets a score of 10 PLUS a bonus for the number
--      of pins knocked down with the first ball of the next frame   
--                          
-- Example scoring:
--      Frame1 ball 1 = 7, ball 2 = 3
--      Frame2 ball 1 = 4, ball 2 = 2
--      Frame 1 scores (7+3+4) 14, Frame 2 scores (4+2) 6
--      Total score = 14 + 6 = 20
--
bowlingTest2 :: ([ Int ] -> Int) -> Test
bowlingTest2 score = 
   "one spare" ~: 20 ~=? score ([7,3,4,2] ++ (replicate 16 0))

score2 :: [ Int ] -> Int
score2 = score where
   score []                       = 0
   score (x:y:z:zs) | x + y == 10 = 10 + z + score2 (z:zs)
   score xs                       = sum xs

score2a :: [ Int ] -> Int
score2a = score where
   score = score2

-- test all three cases with new scoring function
step2 :: IO Counts
step2 = runTestTT (TestList [bowlingTest0 score2, 
                             bowlingTest1 score2, 
                             bowlingTest2 score2])   
                             
-- Scoring for a strike
--      If all pins are knocked down with one ball, score 10
--      plus a bonus of the next two balls
--
-- Example Scoring:
--      Frame 1, ball 1 = 10
--      Frame 2, ball 1 = 3, ball 2 = 6
--      Score for frame 1 is (10+3+6) 19, frame 2 (3+6) 9  
--      Total score = 19 + 9 = 28
--                              
bowlingTest3 :: ([ Int ] -> Int) -> Test
bowlingTest3 score = 
   "one strike" ~: 28 ~=? score ([10,3,6] ++ (replicate 16 0))

score3 :: [ Int ] -> Int
score3 = score where
   score []                       = 0
   score (x:y:z:zs) | x == 10     = 10 + y + z + score3 (y:z:zs)
   score (x:y:z:zs) | x + y == 10 = 10 + z + score3 (z:zs)
   score xs                       = sum xs

step3 :: IO Counts
step3 = runTestTT (TestList [bowlingTest0 score3, 
                             bowlingTest1 score3, 
                             bowlingTest2 score3,
                             bowlingTest3 score3])   

-- Scoring a Perfect Game
--      A perfect game is rolling 12 strikes in a row     
--      The maximum score one can get is 300
--                        
bowlingTest4 :: ([ Int ] -> Int) -> Test
bowlingTest4 score = "perfect game" ~: 300 ~=? score (replicate 12 10) 

score4 :: [ Int ] -> Int
score4 = score where
    score []                              = 0
    score xs         | isPerfect xs       = 300
    score (x:y:z:zs) | x         == 10    = 10 + y + z + score3 (y:z:zs)
    score (x:y:z:zs) | x + y     == 10    = 10 + z + score3 (z:zs)
    score xs                              = sum xs
    
    isPerfect xs = length xs == 12 && sum xs == 120
                     
step4 :: IO Counts                     
step4 = runTestTT (TestList [bowlingTest0 score4, 
                             bowlingTest1 score4, 
                             bowlingTest2 score4,
                             bowlingTest3 score4,
                             bowlingTest4 score4])   
     
test1 :: Test
test1 = TestList (map checkOutput scores) where
  -- the five test cases, in order 
  bowlingTests  = [bowlingTest0, bowlingTest1, bowlingTest2, 
                   bowlingTest3, bowlingTest4]
 
  -- the names of the score functions, the functions themselves, 
  -- and the expected number of passing tests
  scores = zip3 ['0' ..] [score0, score1, score2a, score3, score4] [1..]
 
  -- a way to run a unit test without printing output 
  testSilently = performTest (\ _ _ -> return ()) 
                   (\ _ _ _ -> return ()) (\ _ _ _ -> return ()) ()
 
  -- run each bowling test on the given score function, making sure that 
  -- the expected number of tests pass.
  checkOutput (name, score, pass) = " Testing score" ++ [name] ~: do 
    (s0,_) <- testSilently (TestList $ bowlingTests <*> [score])
    assert $ pass @=? cases s0 - (errors s0 + failures s0)

step5 :: IO Counts
step5 = runTestTT test1     -- passes
    
-- Problem 2

-- 2a)

-- | The conv function takes two lists of numbers, reverses the 
-- second list, multiplies their elements together pointwise, and sums
-- the result.  This function assumes that the two input
-- lists are the same length.
 
conv :: [Int] -> [Int] -> Int
conv [] []  = error "conv: empy lists"
conv xs ys = sum $ zipWith (*) xs (reverse ys)


t2a :: Test
t2a = "2a" ~: conv [2,4,6] [1,2,3] ~?= 20

-- 2b) 

-- | The normalize function adds extra zeros to the beginning of each
-- list so that they each have exactly length 2n, where n is 
-- the length of the longer number.   
 
normalize :: [Int] -> [Int] -> ([Int], [Int])
normalize xs ys = ( pad (maxLen - lenXs) ++ xs, 
                    pad (maxLen - lenYs) ++ ys ) 
    where
        lenXs  = length xs
        lenYs  = length ys
        maxLen = 2 * (max lenXs lenYs)
        
        pad len = replicate len 0

t2b :: Test
t2b = "2b" ~: normalize [1] [2,3] ~?= ([0,0,0,1], [0,0,2,3])

-- 2c)

-- | multiply two numbers, expressed as lists of digits, using 
-- the Ūrdhva Tiryagbhyām algorithm.

-- Note: suspect there's a more elegant way to implement this
--       method
 
multiply :: [Int] -> [Int] -> [Int]
multiply n1 n2 = tail . mul 0 . map (\(x,y) -> conv x y)
               $ groupDigits (reverse (fst norm)) 
                             (reverse (snd norm)) ([],[])
    where
        mul :: Int -> [Int] -> [Int]    
        mul _  []    = []
        mul c (x:xs) = mul (div (x+c) 10) xs ++ [mod (x+c) 10]
    
        groupDigits :: [Int] -> [Int] -> ([Int],[Int]) -> [([Int],[Int])]
        groupDigits [] [] prev         = [prev]
        groupDigits (x:xs) (y:ys) prev = new : (groupDigits xs ys new)
            where new = (x : (fst prev), y : (snd prev) )
        groupDigits _ _ _ = error "groupDigits: invalid arguments"
            
        norm :: ([Int],[Int])
        norm = normalize n1 n2    

t2c :: Test
t2c = "2c" ~: multiply [9,4,6][9,2,3] ~?= [8,7,3,1,5,8]

-- 2d) (Warning, this one may be tricky!

convAlt :: [Int] -> [Int] -> Int
convAlt xs ys = conv xs ys

t2d :: Test
t2d = "2d" ~: convAlt [2,4,6][1,2,3] ~=? 20

test2 :: Test
test2 = TestList [t2a,t2b,t2c,t2d]

-- Problem 3
--  Define, debug and test the following functions. (Some of these 
--  functions are part of the Haskell standard prelude or standard 
--  libraries like Data.List.) Their solutions are readily available 
--  online. You should not use these resources, and instead, implement
--  them yourself.
--
--  For each part of the assignment, you need to declare the type of the
--  function, define it, and replace the testcase for that part based on 
--  the problem description. The declared type of each function should be 
--  the most general one. Make sure to test the functions with multiple 
--  inputs using TestList.

test3 :: Test
test3 = "test3" ~: TestList [t3a, t3b, t3ba, t3c, t3ca, t3cb,
                             t3d, t3da, t3e, t3ea, t3eb,
                             t3f, t3fa, t3g, t3ga, t3gb, t3gc, 
                             t3h, t3ha, t3hb, t3hc, t3hd, t3he] -- passes
-- 3 (a)

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse :: a -> [a] -> [a]
intersperse _ []     = []
intersperse _ [x]    = [x]
intersperse e (x:xs) = x : e : intersperse e xs

t3a :: Test
t3a = "3a" ~: intersperse ',' "abcde" ~?= "a,b,c,d,e"

-- 3 (b)

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")] 
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--    
invert :: [(a,b)] -> [(b,a)]
invert = map (\(a,b) -> (b,a))
 
t3b, t3ba :: Test
t3b  = "3b"  ~: invert [("a",1),("a",2)] ~?= [(1,"a"),(2,"a")] 
t3ba = "3ba" ~: invert ([] :: [(Int,Char)])  ~?= []

-- 3 (c)

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) | p x       = x : takeWhile p xs
                   | otherwise = []
 
t3c,t3ca,t3cb :: Test
t3c  = "3c"  ~: takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2]
t3ca = "3ca" ~: takeWhile (< 9) [1,2,3] ~?= [1,2,3]
t3cb = "3cb" ~: takeWhile (< 0) [1,2,3] ~?= []

-- 3 (d)

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3
find :: (a -> Bool) -> [a] -> Maybe a
find _ []                 = Nothing
find p (x:xs) | p x       = Just x
              | otherwise = find p xs
 
t3d, t3da :: Test
t3d  = "3d"  ~: find odd [0,2,3,4] ~?= Just 3
t3da = "3da" ~: find odd [] ~?= Nothing
 
-- 3 (e)

-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p
             
t3e, t3ea, t3eb :: Test
t3e  = "3e"  ~: all odd [1,2,3] ~?= False
t3ea = "3ea" ~: all odd []      ~?= True
t3eb = "3eb" ~: all odd [1,3,5] ~?= True
 
-- 3 (f)

-- map2 f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _          = []
map2 _ _ []          = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

t3f, t3fa :: Test
t3f = "3f"   ~: map2 max [1,2,3] [0,1,2] ~?= [max 1 0, max 2 1, max 3 2]
t3fa = "3fa" ~: map2 max [1,2,3] [0,1,2] ~?= [1, 2, 3]

-- 3 (g)

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]
zip :: [a] -> [b] -> [(a,b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

t3g,t3ga, t3gb, t3gc :: Test
t3g  = "3g"  ~: zip [1,2] [True]           ~?= [(1,True)]
t3ga = "3ga" ~: zip [1,2] ([] :: [Int])    ~?= []
t3gb = "3gb" ~: zip ([] :: [Bool]) [True]  ~?= []
t3gc = "3gc" ~: zip [True] [1,2]           ~?= [(True, 1)]

-- 3 (h)  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored.
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--
-- NOTE: this is neither elegant nor efficient
--       assumes elems are rows, length of an element is # of columns
--       Check Prelude definition
transpose :: [[a]] -> [[a]]
transpose lst = trsp sqrMat
    where
        trsp []       = []
        trsp (xs:xss) = cols xs (trsp xss)
        
        -- turn the matrix into a square matrix
        sqrMat        = map (take maxElems)  lst
        maxElems      = minimum $ map length lst 
        
        cols []    _        = []
        cols col  []        = toRows col
        cols (y:ys)(rs:rss) = (y:rs) : (cols ys rss)
       
        toRows [] = []
        toRows (z:zs) = [z] : toRows zs

t3h, t3ha, t3hb, t3hc, t3hd, t3he :: Test
t3h  = "3h"  ~: transpose [[1,2,3],[4,5,6]]   ~?= [[1,4],[2,5],[3,6]]
t3ha = "3ha" ~: transpose [[1,2,3],[4,5]]     ~?= [[1,4],[2,5]]
t3hb = "3hb" ~: transpose [[1,4],[2,5],[3,6]] ~?= [[1,2,3],[4,5,6]]
t3hc = "3hc" ~: transpose [[1,2,3]]           ~?= [[1],[2],[3]]
t3hd = "3hd" ~: transpose [[1],[2],[3]]       ~?= [[1,2,3]]
t3he = "3he" ~: transpose ([] :: [[Int]])     ~?= []

-- alternative from
-- https://github.com/Yuhta/cis552/blob/master/hw1/Main.hs
transpose'' :: [[a]] -> [[a]]
transpose'' [] = []
transpose'' xss | any null xss = []
                | otherwise    = map head xss 
                               : (transpose'' . map tail $ xss)

-- Data.List definition of transpose, uses list comprehensions
-- to build sublists
transpose'               :: [[a]] -> [[a]]
transpose' []             = []
transpose' ([]   : xss)   = transpose' xss
transpose' ((x:xs) : xss) = 
    (x : [h | (h:_) <- xss]) : transpose' (xs : [ t | (_:t) <- xss]) 


-- Problem 4
-- Find the longest common subsequence between two strings
-- The longest common subsequence is the longest sequence of characters 
-- that occurs, in the same order, in both of the arguments.
--
-- Reference: "Algorithms Unlocked" by Thomas H. Cormen, p117
--

lcs :: String -> String -> String 
lcs [] _  = []
lcs _ []  = []
lcs xs ys | last xs == last ys = lcs (init xs) (init ys) ++ [last xs]
          | otherwise = let a = lcs (init xs) ys
                            b = lcs xs (init ys)
                        in if length a > length b
                           then a else b        

test4 :: Test
test4 = "4" ~: TestList [ lcs "Advanced" "Advantaged" ~?= "Advaned",
                          test4a ]

-- either test4a or test4b is valid; depends which LCS is returned                          
test4a :: Test
test4a = "4a" ~: lcs "abcd" "acbd" ~?= "acd"

test4b :: Test
test4b = "4b" ~: lcs "abcd" "acbd" ~?= "abd"



