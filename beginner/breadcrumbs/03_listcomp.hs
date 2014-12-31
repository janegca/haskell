-- an alternate way of expressing lists

-- generate natural numbers
naturals = [ x | x <- [1..] ]

perfect_squares = [ x*x | x <- naturals ]

-- generate a list of numbers that are multiples of 3
-- the predicate expression after the comma (,) acts as a filter
-- only the values of 'x' which are evenly divisible by 3
-- are included in the final result list
multiples_of_3 = [ x | x <- naturals, x `mod` 3 == 0 ]

-- acts like a nested loop with 'x' being outer loop and 'y'
-- being the inner loop i.e. get 1 paired with 5 thru 10 followed
-- by 2 paired with 5 thru 10, followed by 3 paired with 5 thru 10,
-- etc.
all_pairs = [ (x,y) | x <- [1..5], y <- [5..10] ]

{-
    Example output:
    
    *Main> take 10 naturals
    [1,2,3,4,5,6,7,8,9,10]
    *Main> take 10 perfect_squares
    [1,4,9,16,25,36,49,64,81,100]
    *Main> take 10 multiples_of_3
    [3,6,9,12,15,18,21,24,27,30]
    *Main> all_pairs
    [(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),
     (2,5),(2,6),(2,7),(2,8),(2,9),(2,10),
     (3,5),(3,6),(3,7),(3,8),(3,9),(3,10),
     (4,5),(4,6),(4,7),(4,8),(4,9),(4,10),
     (5,5),(5,6),(5,7),(5,8),(5,9),(5,10)]
    *Main>
-}