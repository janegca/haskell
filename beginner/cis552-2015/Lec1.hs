module Lec1 where
{-
    CIS 552: Advanced Programming (2015)
    Lecture 1 - Introduction
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/Lec1.html

-}
import Test.HUnit

pos :: Int -> Bool
pos x = (x > 0)

arith :: Int -> Int -> Int -> Int
arith x y z = x * (y + z)

plus :: Int -> Int -> Int
-- infix operator given regular name
plus = (+)

p0 :: Int
-- use infix operator a regular function
p0 = (+) 2 ((*) 3 4)

p1 :: Int
-- use name function as infix operator
p1 = 2 `plus` 2

-- tuples can contain different types
t1 = ('a', 5)        :: (Char, Int)
t2 = ('a', 5.2, 7)   :: (Char, Double, Double)
t3 = ((7,5.2), True) :: ((Int,Double), Bool)

-- using pattern matching to extract values from tuples
pat' :: (Int, Int, Int) -> Int
pat' (x,y,z) = x * (y + z)

patEval = pat' (1,2,3)      -- 5

-- optional values
m1 :: Maybe Int
m1 = Just 2

m2 :: Maybe Int
m2 = Nothing

-- extracting values from Maybe's
pat'' :: Maybe Int -> Int
pat'' (Just x) = x
pat'' Nothing  = 2      -- can assign it any Int

-- patterns can be nested
join' :: Maybe (Maybe a) -> Maybe a
join' (Just (Just x)) = Just x
join' (Just Nothing)  = Nothing
join' Nothing         = Nothing

-- easier way to write the above
join'' :: Maybe (Maybe a) -> Maybe a
join'' (Just x) = x
join'' _        = Nothing

-- Maybe is useful for partial functions
location :: String -> Maybe String
location "cis501" = Just "Wu & Chen"
location "cis502" = Just "Heilmeier"
location "cis520" = Just "Wu & Chen"
location "cis552" = Just "Towne 311"
location _        = Nothing

-- IO
-- can compile with
--       ghc -o Lec1 --make -main-is Lec1 Lec1.hs
main :: IO ()
main = putStrLn ("Hello")




