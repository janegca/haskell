-- Hailstone sequence

-- Source: http://www.seas.upenn.edu/~cis194/lectures/01-intro.html

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
    where
        hailstone :: Integer -> Integer
        hailstone n
          | n `mod` 2 == 0 = n `div` 2
          | otherwise      = 3*n + 1

-- return the number of steps in a hailstone sequence given the
-- starting number       
hailstoneLen :: Integer -> Int
hailstoneLen n = length (hailstoneSeq n) - 1
