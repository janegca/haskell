-- Informatics 1 - Functional Programming 
-- Tutorial 3 - Higher Order Functions
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/tutorial3.zip
--
{-
    General
    
    Haskell functions are values, which may be processed in the same way 
    as other data such as numbers, tuples or lists. In this tutorial 
    we'll use a number of higher-order functions, which take other 
    functions as arguments, to write succinct definitions for the sort of
    list-processing tasks that you've previously coded explicitly using 
    recursion or comprehensions.
-}

import Data.Char
import Data.List
import Test.QuickCheck

{-
    MAP
    
    Transforming every list element by a particular function is a common 
    need when processing lists for example, we may want to
    
        - add one to each element of a list of numbers,
        - extract the first element of every pair in a list,
        - convert every character in a string to uppercase, or
        - add a grey background to every picture in a list of pictures.
        
    The map function captures this pattern, allowing us to avoid the 
    repetitious code that results from writing a recursive function for 
    each case.
    
    Consider a function g defined in terms of an imaginary function f as 
    follows:
    
        g []     = []
        g (x:xs) = f x : g xs
        
    The function g can be written with recursion (as above), or with a 
    comprehension, or with map: all three definitions are equivalent.
    
        g xs = [ f x | x <- xs ]
        g xs = map f xs
        
    Below right is the definition of map. Note the similarity to the 
    recursive definition of g (below left).
    As compared with g, map takes one additional argument: the function f 
    that we want to apply to each element.
    
                                map :: (a -> b) -> [a] -> [b]
        g []     = []           map f []     = []
        g (x:xs) = f x : g xs   map f (x:xs) = f x : map f xs
        
    Given map and a function that operates on a single element, we can 
    easily write a function that operates on a list. For instance, the 
    function that extracts the first element of every pair can be
    defined as follows (using fst :: (a,b) -> a):
    
        fsts :: [(a,b)] -> [a]
        fsts pairs = map fst pairs
-}
-- 1. Map 
--      Using map and other suitable library functions, write definitions 
--      for the following:

-- (a) A function that converts a string to upper case
uppers :: String -> String
uppers = map toUpper

-- (b) A function that doubles every element in a list
doubles :: [Int] -> [Int]
doubles = map (*2)

-- (c) A function that turns prices given in cents to dollars     
--  See provided solution; 
--      (/ 100), (`div` 100) and (* 0.01) all raise type errors
--
centsToDollars :: [Int] -> [Float]
centsToDollars = map toDollars
    where toDollars cents = (fromIntegral cents) * 0.01

-- (d) Write a list-comprehension version of uppers and use it to
--     check your answer to (a)
uppers' :: String -> String
uppers' xs  = [ toUpper x | x <- xs ]

prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppers' xs

{- 2. FILTER

    Removing elements from a list is another common need. For example, 
    we might want to remove non-alphabetic characters from a string, or
    negative integers from a list. This pattern is captured by the filter
    function.
    
    Consider a function g defined in terms of an imaginary predicate p 
    as follows (a predicate is just a function with a Bool value):
    
        g [] = []
        g (x:xs) | p x = x : g xs
                 | otherwise = g xs

    The function g can be written with recursion (as above), or with a 
    comprehension, or with filter: all three definitions are equivalent.
    
        g xs = [ x | x <- xs, p x ]
        g xs = filter p xs
        
    For instance, we can write a function evens :: [Int] -> [Int], 
    which removes all odd numbers from a list using filter and the 
    standard function even :: Int -> Int:
    
            evens list = filter even list
            
    This is equivalent to:
    
            evens list = [x | x <- list, even x]
            
    Below right is the definition of filter. Note the similarity to the 
    way g is defined (below left). As compared with g, filter takes one 
    additional argument: the predicate that we use to test each element.
    
                                  filter :: (a -> Bool) -> [a] -> [a]
      g [] = []                   filter p [] = []
      g (x:xs) | p x = x : g xs   filter p (x:xs) | p x = x : filter p xs
               | otherwise = g xs                 | otherwise = filter p xs
    
-}
-- 2. Using filter and other standard library functions, write 
--    definitions for the following:
--
-- (a) A function that removes all non-alphabetic characters from a string
alphas :: String -> String
alphas = filter isAlpha

-- (b) Remove all occurrences of a character from a string
rmChar ::  Char -> String -> String
rmChar ch = filter (not . (== ch))

-- (c) A function that removes all numbers less than or equal to a
--     given number.
above :: Int -> [Int] -> [Int]
above n = filter (> n)

-- (d) A function that removes all pairs with equal values
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter notEq
    where notEq (x,y) =  x /= y

-- (e) Write a list comprehension version of rmChar and use QuickCheck
--     to test it against the (b) version
rmCharComp :: Char -> String -> String
rmCharComp ch str = [ x | x <- str, x /= ch]

prop_rmChar :: Char -> String -> Bool
prop_rmChar ch str = rmChar ch str == rmCharComp ch str


{- 3. Comprehensions vs. map & filter
    
    As we have seen, list comprehensions process a list using 
    transformations similar to map and filter. In general, 
    [f x | x <- xs, p x] is equivalent to map f . (filter p xs).
    
    Write expressions equivalent to the following using map and filter. 
    Use QuickCheck to verify your answers.
    
    [Note: function composition with map, filter and sections REALLY
           shortens code; most of these functions would be quite a
           bit longer if written in Java, C, or any other imperative 
           language. ]
-}
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = map toUpper . (filter isAlpha)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (2 *) . filter (> 3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter (even . length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs

{- 4. Fold
    
    The map and filter functions act on elements individually; they never 
    combine one element with another.
    
    Sometimes we want to combine elements using some operation. For 
    example, the sum function can be written like this:

        sum [] = 0
        sum (x:xs) = x + sum xs
        
    Here we're essentially just combining the elements of the list using 
    the + operation. Another example is reverse, which reverses a list:
    
        reverse [] = []
        reverse (x:xs) = reverse xs ++ [x]

    This function is just combining the elements of the list, one by one,
    by appending them onto the end of the reversed list. This time the 
    "combining" function is a little harder to see. It might be easier 
    if we wrote it this way:
    
        reverse [] = []
        reverse (x:xs) = x `snoc` reverse xs
        
        snoc x xs = xs ++ [x]
        
    Now you can see that `snoc` plays the same role as + played in the 
    example of sum.
    
    These examples (and many more) follow a pattern: we break down a list 
    into its head (x) and tail (xs), recurse on xs, and then apply some 
    function to x and the modified xs. The only things we need to specify
    are the function (such as (+) or snoc) and the initial value (such as 
    0 in the case of sum and [] in the case of reverse.
    
    This pattern is called "a fold" and is implemented in Haskell via the 
    function foldr.
    
                                foldr :: (a -> b -> b) -> b -> [a] -> b
        g [] = u                foldr f u []     = u
    g (x:xs) = x `f` g xs       foldr f u (x:xs) = x `f` foldr f u xs
    
    The function g can be written with recursion (as above) or by using 
    a fold: both definitions are equivalent.
    
            g xs = foldr f u xs
    
    Where 'u' (sometimes called 'a unit') is the starting value of type
    'b' and the list is of type [a] i.e. a : (a1 : (a2 : ( a3 : []))) and
    'f' is a function; we replace every 'cons' (:) operator in the
    list with the function 'f' and the empty list ([]) with the start
    value 'u' to get:
    
        a `f` (a1 `f` (a2 `f` (a3 `f` []))) 
        
    For example, we can define sum :: [Int] -> Int as follows, using (+) 
    as the function and 0 as the initial value (unit):
    
        sum :: [Int] -> Int
        sum ns = foldr (+) 0 ns
    
    (Note: to treat an infix operator like + as a function name, we need
           to wrap it in parentheses.)
           
    We will practise the use of foldr by writing several functions first
    with recursion, and then using foldr. You can use other standard 
    library functions as well. For each pair of functions that you write, 
    test them against each other using QuickCheck.
    
-}
-- (a) Look at the recursive function productRec :: [Int] -> Int that 
--     computes the product of the numbers in a list, and write an 
--     equivalent function productFold using foldr.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- (b) Write a recursive function andRec :: [Bool] -> Bool that checks 
--     whether every item in a list is True. Then, write the same 
--     function using foldr, this time called andFold.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- (c) Write a recursive function concatRec :: [[a]] -> [a] that 
--     concatenates a list of lists into a single list. Then, write a 
--     similar function concatFold using foldr.
concatRec :: [[a]] -> [a]
concatRec []     = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

{- d. Write a recursive function 
        rmCharsRec :: String -> String -> String 
        
      that removes all characters in the first string from the second 
      string, using your function rmChar from exercise (2b).
      
        *Main> rmCharsRec ['a'..'l'] "football"
        "oot"

      Then, write a second version rmCharsFold using rmChar and foldr. 
      Check your functions with QuickCheck.

-}
rmCharsRec :: String -> String -> String
rmCharsRec [] ys     = ys
rmCharsRec (x:xs) ys = rmCharsRec xs (rmChar x ys)

rmCharsFold :: String -> String -> String
-- The trick to this was recognizing the 'starting value' was
-- the second string rather than an empty string
rmCharsFold xs ys = foldr rmChar ys xs 

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


{-
    MATRIX MULTIPLICATION
    
    Next, we will look at matrix addition and multiplication. As matrices 
    we will use lists of lists of Ints; for example:
    
        [[1,4,9],   represents a 2x3 matrix
         [2,5,7]]
         
    The declaration below makes the type Matrix a shorthand for the type 
    [[Int]]:
    
        type Matrix = [[Int]]
        
    Our first task is to write a test to show whether a list of lists of 
    Ints is a matrix. This test should verify two things: 
        1) that the lists of Ints are all of equal length, and 
        2) that there is at least one row and one column in the list of 
           lists.
-}

type Matrix = [[Int]]       -- type synonym for a Matrix

{- 5 
  (a) Write a function 
            uniform :: [Int] -> Bool 
      
      that tests whether the integers in a list are all equal. You can 
      use the library function 'all', which tests whether all the elements
      of a list satisfy a predicate; check the type to see how it is used. 
      If you want, you can try to define 'all' in terms of foldr and map.
      
      [Note: this is a helper function, a list will be populated with the
             lengths of each matrix element and then checked to see that
             all element values, and therefore row lengths, are equal,
             which in turn means every row has the same number of columns.]
        
-}
-- provided solution
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (&&) True . map p

uniform :: [Int] -> Bool
uniform xs = all (== (head xs)) (tail xs)

{-
    (b) Using your function uniform write a function 
            valid :: Matrix -> Bool 
            
        that tests whether a list of lists of Ints is a matrix (it should 
        test the properties 1) and 2) specified above).
        
        [Note:  # of rows = # of elements
                # of cols = length of element
                [[a]] = 1 row, 1 col
        ]
-}
-- provided solution
valid :: Matrix -> Bool
valid []     = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))

{-
    ZIPWITH
    
    A useful higher-order function is zipWith. It is a lot like the 
    function zip that you have seen, which takes two lists and combines 
    the elements in a list of pairs. The difference is that instead of
    combining elements as a pair, you can give zipWith a specific function
    to combine each two elements. The definition is as follows (Figure 4 
    gives an illustration):
    
        zipWith f [] _ = []
        zipWith f _ [] = []
        zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
        
    UNCURRY
    
    Another useful function for working with pairs is uncurry, which 
    turns a function that takes two arguments into a function that 
    operates on a pair.
-}

{- 6.
   (a)  Look up the definition of uncurry. What is returned by the 
        following expression?
                Main> uncurry (+) (10,8)
                
        Defined in Prelude as:
            uncurry :: (a -> b -> c) -> ((a, b) -> c)
            uncurry f p =  f (fst p) (snd p)

        So, 'uncurry (+) (10,8) will return 18'
        
    (b) Show how to define zipWith using zip and a list comprehension.
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ x `f` y | (x,y) <- zip xs ys]

{-
    (c) Show how to define zipWith using zip and the higher-order 
        functions map and uncurry, instead of the list comprehension.
-}
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

{-
    MATRIX ADDITION
    
    Adding two matrices of equal size is done by pairwise adding the 
    elements that are in the same position, i.e. in the same column and 
    row, to form the new element at that position. For example:
    
    1 2 3   +   10 20 30  = 11 22 33
    4 5 6       40 50 60    44 55 66

    We will use zipWith to implement matrix addition.
-}

{- 7. Write a function plusM that adds two matrices. Return an error 
      if the input is not suitable. It might be helpful to define a helper
      function plusRow that adds two rows of a matrix.
-}
plusM :: Matrix -> Matrix -> Matrix
plusM (x:xs) (y:ys) = plusRow x y : plusM xs ys
    where plusRow x y | length x == length y = zipWith (+) x y
                      | otherwise = error ("unequal row lengths")
plusM _ _ = []      

-- alternative based on provided solution
plusM' :: Matrix -> Matrix -> Matrix
plusM' m1 m2 | ok        = zipWith (zipWith (+)) m1 m2
             | otherwise = error("invalid input")
    where
        ok = valid m1 && valid m2

testPlus = plusM' [[1,2,3],[4,5,6]] [[10,20,30],[40,50,60]] 
           == [[11,22,33],[44,55,66]]   
         && plusM' [[3,-2],[2,-6],[1,5]] [[2,4],[4,0],[-2,2]]
           == [[5,2],[6,-6],[-1,7]]         

{-
    MATRIX MULTIPLICATION
    
    For matrix multiplication we need what is called the dot product or 
    inner product of two vectors:
    
        (a1,a2,..an) x (b1,b2,..bn) = a1b1 + a2b2 + ... + anbn
        
    Matrix multiplication is then defined as follows: two matrices with 
    dimensions (n,m) and (m,p) are multiplied to form a matrix of 
    dimension (n,p) in which the element in row i, column j is the dot
    product of row i in the first matrix and column j in the second. 
    For example:
    
          1 10  x  1 2 =  31  42
        100 10     3 4   130 240
-}      
{- 8. Define a function timesM to perform matrix multiplication. Return 
      an error if the input is not suitable. It might be helpful to define
      a helper function dot for the dot product of two vectors (lists). 
      The function should then take the dot product of the single row with
      every column of the matrix, and return the values as a list. To make
      the columns of a matrix readily available you can use the function 
      transpose (you should remember this function from Tutorial 2).
-}
timesM :: Matrix -> Matrix -> Matrix
timesM (x:xs) mat = dot x (transpose mat) : timesM xs mat
    where
        dot row (c:cs) 
            | length row == length c = sum (zipWith (*) row c) : dot row cs
            | otherwise = error("invalid input")
        dot _ _ = []
timesM _ _ = []

-- alternative based on provided solution
timesM' :: Matrix -> Matrix -> Matrix
timesM' m1 m2 | ok = [ [dot row col | col <- transpose m2] | row <- m1 ]
              | otherwise = error("invalid input")
    where
        dot row col = sum (zipWith (*) row col)
        ok          = valid m1 && valid m2

testMult = timesM' [[1,10],[100,10]] [[1,2],[3,4]] == [[31,42],[130,240]]
         && timesM' [[1,3,2],[-1,2,1],[0,1,0]] [[4,1,2],[1,0,1],[3,1,5]]
           == [[13,3,15],[1,0,5],[1,0,1]]         

{- Optional material

    For a real challenge, you can try to compute the inverse of a matrix. 
    There are a few steps involved in this process:
    
    (a) The entries of a matrix should be changed to Doubles or (even 
        better) Rationals to allow proper division.
        
    (b) You will need a function to find the determinant of a matrix. 
        This will tell you if it has an inverse.
        
    (c) You will need a function to do the actual inversion.
    
    There are several different algorithms available to compute the 
    determinant and the inverse of a matrix. Good places to start looking 
    are:
        http://mathworld.wolfram.com/MatrixInverse.html
        http://en.wikipedia.org/wiki/Invertible_matrix
        
    Finally, implement an appropriate quickCheck test for your function.
-}
-- 9.
{-
    Source:
        'A First Course in Linear Algebra' - Determinants Section
        http://linear.pugetsound.edu/html/section-DM.html
        
    Calculating the determinant of a matrix
    ---------------------------------------
    
    Make sure the matrix is valid (square)
    
    The determinant of a 1x1 matrix [[a]] is a
    
    To calculate the determinants for all other matrices we
    need to:
    
    1. get the first row of the matrix
    2. sum the product of each row element times the determinant
       of its related sub-matrix; multiplying row elements in the 
       odd column positions by -1.
    3  If the value of an element is 0 the result of the element
       times its sub-matrix will be 0 so we can skip the computation
       
    A sub-matrix is equal to the original matrix with the row
    and columns corresponding to the current row element's column
    position, removed
    
    For example,
        1  2  3  4  - row 0
        5  6  7  8  - row 1
        9 10 11 12  - row 2
       15 16 17 18  - row 3
        
     elem *  det of
     --------------
        1 *   6 7 8     - element in column position 0 of row 1
           10 11 12       removed row 0, col 0 from original matrix
           16 17 18
           
       -2 * 1  3  4     - element in column position 1 of row 1
            9 11 12       removed row 1, col 1 from original
           15 17 18
            
        3 * 1  2  4     - element in column position 2 of row 1
            6  7  8       removed row 2, col 2 from original
           15 16 18
           
       -4 * 1  2  3     - element in column position 3 or row 1
            5  6  7       removed row 3, col 3 from original
            9 10 11
            
    Implementation Note:
        this is NOT efficient, only useful for demonstrating
        how the determinant is calculated
-}

det :: Matrix -> Int
det [[x]] = x
det mat   = if valid mat 
            then let t = transpose (tail mat)
                     r = zip (head mat) [0..]
                 in sumMinors r t
            else error("invalid input")
    where      
        sumMinors ((e,p) : xs) m 
            | e == 0    = sumMinors xs m
            | otherwise = mult * e * det(minor p m) + sumMinors xs m
            where
                mult | odd p     = -1
                     | otherwise = 1
        sumMinors _  _ = 0
               
        minor idx m = transpose (removeCol idx m)

        removeCol index mat = let (l,r) = splitAt index mat
                               in l ++ (drop 1 r)
                       
{-
    Ref:
        'A First Course in Linear Algebra'
        http://linear.pugetsound.edu/html/section-MISLE.html
        
    Calculating the inverse of a matrix
    -----------------------------------
    
    A square matrix is invertible if its determinant is  /= 0
    
    TODO

-}                       
-- test matrices        
m1 :: Matrix                -- determinant should equal 4
m1 = [ [1, 0,  1, 1],
       [2, 2, -1, 1],
       [2, 1,  3, 0],
       [1, 1,  0, 1]]
       
m2 :: Matrix                -- determinant should equal 0
m2 = [ [1,  0,  1, 1],
       [2, -1, -1, 1],
       [2,  5,  3, 0],
       [1, -1,  0, 1]]
        
m3 :: Matrix
m3 = [[2]]                 -- determinant should be 2
