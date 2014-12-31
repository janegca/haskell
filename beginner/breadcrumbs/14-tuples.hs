-- tuples offer another way to represent data
-- unlike lists, the elements DO NOT have to all be of the same
-- type

import Data.List (sort)

min_max_list ::  Ord a => [a] -> (a, a)
min_max_list lst = (head slst, head rslst) 
    where
        rslst = reverse slst
        slst  = sort lst

magnitude :: Floating a => (a,a) -> a    
magnitude (x,y) = sqrt (x^2 + y^2)

-- list comprehension to generate pythagorean triples
pythagorean_triples :: Int -> [(Int,Int,Int)]
pythagorean_triples n = [(a,b,c) | c <- [1..n],
                                   a <- [1..c],
                                   b <- [a..c],
                                   a^2 + b^2 == c^2]

-- use the functions 'fst' and 'snd' to return the first or
-- second element
                                   
firstElement pair  = fst pair
secondElement pair = snd pair    

-- or use pattern matching
fstElem (x,y) = x
sndElem (x,y) = y                               

{-
    Example output:
    
    *Main> min_max_list (['a'..'z'] ++ ['A'..'Z'])
    ('A','z')

    *Main> magnitude (2,4)
    4.47213595499958

    *Main> pythagorean_triples 20
    [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
    *Main>     

    *Main> firstElement ("hello","goodbye")
    "hello"
    *Main> secondElement ("hello","goodbye")
    "goodbye"
    *Main>    
    
    *Main> fstElem (2,4)
    2
    *Main> sndElem (2,4)
    4
    *Main>     

-}                                   