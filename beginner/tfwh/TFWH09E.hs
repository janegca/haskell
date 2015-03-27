module TFWH09E where

import System.Random

{-
    Chapter 9 - Infinite Lists - Notes and Exercises
        
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/

-}

-- primes as a cyclic list (a list whose definition is recursive)
-- (\\) subtracts one strictly increasing list from another
primes :: [Integer]
primes = 2:([3..] \\ composites)
    where composites = mergeAll [map (p*) [p..] | p <- primes]

(x:xs) \\ (y:ys) | x<y   = x:(xs \\ (y:ys))
                 | x==y  = xs  \\ ys
                 | x>y   = (x:xs) \\ ys

mergeAll (xs:xss) = xmerge xs (mergeAll xss)

xmerge (x:xs) ys = x:merge xs ys

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x<y  = x:merge xs (y:ys)
                    | x==y = x:merge xs ys
                    | x>y  = y:merge (x:xs) ys
                    
{-
    Paper, Rock, Scissors
-}                    
data Move      = Paper | Rock | Scissors
type Round     = (Move, Move)

score :: Round -> (Int, Int)
score (x,y) | x `beats` y = (1,0)
            | y `beats` x = (0,1)
            | otherwise   = (0,0)
            
beats :: Move -> Move -> Bool
beats Paper Rock     = True
beats Rock Scissors  = True 
beats Scissors Paper = True
beats _ _            = False      

-- following a specific strategy that takes a list of an
-- opponents moves and returns a response
type Strategy1 = [Move] -> Move

copy1 :: Strategy1
-- repeat opponent's last move
copy1 ms = if null ms then Rock else head ms

smart1 :: Strategy1
-- choose a move based on frequency of opponents previous moves
smart1 ms = if null ms then Rock
            else pick (foldr count (0,0,0) ms)
            
count :: Move -> (Int,Int,Int) -> (Int,Int,Int)
count Paper (p,r,s)    = (p+1, r, s)
count Rock  (p,r,s)    = (p, r+1, s)
count Scissors (p,r,s) = (p, r, s+1)
            
pick :: (Int,Int,Int) -> Move
pick (p,r,s) | m < p = Scissors
             | m < p+r = Paper
             | otherwise = Rock
    where m = rand (p + r + s)   

rand :: Int -> Int
rand n = fst $ randomR (0, n-1) (mkStdGen n)    

rounds1 :: (Strategy1, Strategy1) -> [Round]
-- keep track of opponent moves (inefficient)
rounds1 (p1, p2) = map head $ tail $ iterate (extend (p1, p2)) []
    where
        extend (p1,p2) rs = (p1 (map snd rs),
                             p2 (map fst rs)) : rs

match1 :: Int -> (Strategy1, Strategy1) -> (Int,Int)
match1 n = total . map score . take n . rounds1
    where total rs = (sum (map fst rs), sum (map snd rs))
    
ex1 = match1 1000 (copy1, smart1)
         
-- ------------------------------------------------------------------
-- using another strategy

-- a strategy that takes a list of opponent moves and returns
-- a list of responses
type Strategy2 = [Move] -> [Move]

copy2 :: Strategy2
-- return the opponents last move
copy2 ms = Rock : ms

smart2 :: Strategy2
smart2 ms = Rock : map pick (stats ms)
    where 
        -- computes the running count of the 3 possible moves
        stats = tail . scanl (flip count) (0,0,0)
        
rounds2 :: (Strategy2, Strategy2) -> [Round]
rounds2 (p1,p2) = zip xs ys
    where xs = p1 ys
          ys = p2 xs
    
match2 :: Int -> (Strategy2, Strategy2) -> (Int,Int)
match2 n = total . map score . take n . rounds2
    where total rs = (sum (map fst rs), sum (map snd rs))
    
ex2 = match2 1000 (copy2, smart2)
    
cheat ::[Move] -> [Move]
-- guaranteed to beat first and subsequent moves    
cheat ms = map trump ms
    where
        trump Paper    = Scissors
        trump Rock     = Paper
        trump Scissors = Rock

ex3 = match2 1000 (copy2, cheat)
ex4 = match2 1000 (smart2, cheat)

devious :: Int -> Strategy2
-- behave like copy for n moves and then begins to cheat
devious n ms = take n (copy2 ms) ++ cheat (drop n ms)

ex5 = match2 1000 (smart2, devious 100)

police :: Strategy2 -> [Move] -> [Move]
-- force an honest strategy
police p ms = ms' 
    where ms' = p (synch ms ms')
          synch (x:xs) (y:ys) = (seq y x) : synch xs ys
          
rounds3 (p1,p2) = zip xs ys
    where
        xs = police p1 ys
        ys = police p2 xs
        
match3 :: Int -> (Strategy2, Strategy2) -> (Int,Int)
match3 n = total . map score . take n . rounds3
    where total rs = (sum (map fst rs), sum (map snd rs))
        
ex6 = match3 1000 (smart2, devious 100)  -- no termination
ex7 = match3 1000 (copy2, cheat)         -- no termination
ex8 = match3 1000 (copy2, smart2)        -- (312,328)
       
-- -----------------------------------------------------------------
-- Doubly linked lists
-- -----------------------------------------------------------------
data DList a = Cons a (DList a) (DList a)

instance Show a => Show (DList a)
    where show d = show (elem' d)

elem' :: DList a -> a
elem' (Cons a p n) = a

prev, next' :: DList a -> DList a
prev (Cons a p n) = p
next' (Cons a p n) = n
    
mkCDList :: [a] -> DList a
-- create a doubly linked list from a list    
mkCDList as = head xs
    where xs = zipWith3' Cons as (rotr xs) (rotl xs)
    
          -- tilde operator forces lazy pattern evaluation
          -- y and z will only be evaluated when x is needed
          zipWith3' f (x:xs) ~(y:ys) ~(z:zs)
            = f x y z : zipWith3' f xs ys zs
          zipWith3' _ _ _ _  = []
          
          rotr xs = [last xs] ++ init xs
          rotl xs = tail xs ++ [head xs]
          
          
p1 = Cons "Page 1" p3 p2
p2 = Cons "Page 2" p1 p3
p3 = Cons "Page 3" p2 p1    

book = [p1, p2, p3]
currPage = mkCDList book
nPage    = next' currPage
pPage    = prev nPage

{-
    *TFWH09E> book
    ["Page 1","Page 2","Page 3"]
    *TFWH09E> currPage
    "Page 1"
    *TFWH09E> nPage
    "Page 2"
    *TFWH09E> pPage
    "Page 1"
    *TFWH09E> 

-}

-- -----------------------------------------------------------------
-- Exercises

-- Exercise B
cycle' :: [a] -> [a]
cycle' [] = error "empty list"
cycle' xs = xs' where xs' = xs ++ xs'

exBa = take 20 (cycle' "hallo")
exBb = cycle' []

-- Exercise C (provided solution)
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

exC = take 20 fibs

-- Exercise D (provided solution)
--      Produces an infinite list of numbers with the following
--      three properties:
--      1. the list is in strictly increasing order
--      2. the list begins with the number 1
--      3. if the list contains the number x then it contains
--         the numbers 2x, 3x, 5x
--      4. the list contains no other numbers
--
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming)
                (merge (map (*3) hamming)
                       (map (*5) hamming))
                       
exD = take 20 hamming
                       
-- Exercise J - Sundaram Primes
--      uses the fact that no prime is ever equal to
--      (2 * i + 1) * (2 * j + 1)
--
sprimes = 2 : [2 * n+1 | n <- [1..] \\ sundaram ]
    where  
        -- sundaram represents numbers NOT to be included
        -- in the primes list
        sundaram = 
                mergeAll [ [i + j + 2 * i * j | j <- [i..] ]
                          | i <- [1..] ]
                          
exJ = take 20 sprimes

-- Exercise L 
--      A 'torus' is a doubly-cyclic, doubly-double-linked list 
--      It is doubly-cyclic linked list both left to right
--      and top to bottom. Given a matrix as a list of length
--      m of lists, all of length n, construct a definition
--              mkTorus :: Matrix a -> Torus a
--      where
--          the definition, given below, hold

data Torus a = Cell a (Torus a) (Torus a) (Torus a) (Torus a)
    deriving Show
    
type Matrix a = [[a]]

telem (Cell a _ _ _ _) = a
up    (Cell _ u _ _ _) = u
down  (Cell _ _ d _ _) = d
left  (Cell _ _ _ l _) = l
right (Cell _ _ _ _ r) = r        

-- from provided solution
--   links are on individual cells, not rows and columns
mkTorus :: Matrix a -> Torus a
mkTorus ass = head (head xss)
    where xss = zipWith5' (zipWith5' Cell)
                    ass (rotr xss) (rotl xss)
                    (map rotr xss) (map rotl xss)
    
          zipWith5' f (v:vs) ~(w:ws) ~(x:xs) ~(y:ys) ~(z:zs)
            = f v w x y z : zipWith5' f vs ws xs ys zs
          zipWith5' _ _ _ _ _ _  = []
          
          rotr r = [last r] ++ init r
          rotl r = tail r ++ [head r]
                          
m  = [[1,2,3], [4,5,6], [7,8,9], [10,11,12]]
mt = mkTorus m
