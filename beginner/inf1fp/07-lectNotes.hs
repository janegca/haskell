-- Informatics 1 - Functional Programming 
-- Lecture 7 - Map, Filter, Fold

import Test.QuickCheck
import Data.Char (ord, isDigit)

{-
    Common computational patterns
        - learning to recognize them and generalize them
        - trick to this is using higher-order functions
-}
squares :: [Int] -> [Int]
squares xs = [ x*x | x <- xs ]

squares' :: [Int] -> [Int]
squares' [] = []
squares' (x:xs) = x*x : squares xs

-- getting the ordinal (ASCII value) of a character
ords :: [Char] -> [Int]
ords xs = [ ord x | x <- xs ]

ords' :: [Char] -> [Int]
ords' [] = []
ords' (x:xs) = ord x : ords xs

{-
    The above definitions have the same 'shape'; what changes
    is the operation:
        squares x*x
        ords    (ord x)
        
    Basically, the same function is applied to every element in
    a list, 'transforming' the element. 
    
    This basic pattern is generalized in 'map'

        map :: (a -> b) -> [a] -> [b]
        map f xs = [ f x | x <- xs ]

        map :: (a -> b) -> [a] -> [b]
        map f [] = []
        map f (x:xs) = f x : map f xs


    Type signature: (a -> b) -> [a] -> [b]
        (a -> b)     declares a function that takes elements of type
                     'a' and transforms them into elements of type 'b'
        [a]          list of any type
        [b]          list of original 'a' elements transformed to 'b'
                     values (both may be of same type or different)
                     
    You can take functions as arguments, return functions as results
    (any function that does this is called a 'higher-order function')
    
    Can rewrite squares and ords using map
-}
mapSquares :: [Int] -> [Int]
mapSquares xs = map sqr xs
    where
    sqr x = x*x

{-
    Walkthrough
        mapSquares [1,2,3]
        ==> map sqr (1 : (2 : (3 [])))
        ==> sqr 1 : map sqr (2 : (3 []))
        ==> sqr 1 : (sqr 2 : map sqr (3 : []))
        ==> sqr 1 : (sqr 2 : sqr 3 : map sqr []))
        ==> sqr 1 : (sqr 2 : (sqr 3 : [])
        ==> 1 : ( 4 : (9 []))
        ==> [1,4,9]
        
    Ords can be re-written using 'map'
-}    
mapOrds :: [Char] -> [Int]
mapOrds xs = map ord xs

{-
    Filtering is another pattern of computation where we extract
    values from a list based on some property.
    
    One example is 'positives' where we extract only numbers
    greater than zero
    
    Another example is 'digits' which returns only those characters
    in a list which are digit numbers
-}
positives :: [Int] -> [Int]
positives xs = [ x | x <- xs, x > 0 ]

positives' :: [Int] -> [Int]
positives' [] = []
positives' (x:xs) | x > 0 = x : positives xs
                  | otherwise = positives xs
                          
digits :: [Char] -> [Char]
digits xs = [ x | x <- xs, isDigit x ]

digits' :: [Char] -> [Char]
digits' [] = []
digits' (x:xs) | isDigit x = x : digits xs
               | otherwise = digits xs                          

{-
    Both positives and digits have the same pattern, the only
    difference is in the guards (predicates):
        x > 0
        isDigit x
        
    This pattern can be extracted and generalized in 'filter'
    
        filter :: (a -> Bool) -> [a] -> [a]
        filter p xs = [ x | x <- xs, p x ]
        
        filter :: (a -> Bool) -> [a] -> [a]
        filter p [] = []
        filter p (x:xs) | p x = x : filter p xs
                        | otherwise = filter p xs
                        
    The functions positives and digits can be rewritten using filter
-}               
filterPositives :: [Int] -> [Int]
filterPositives xs = filter pos xs
    where
    pos x = x > 0

filterDigits :: [Char] -> [Char]
filterDigits xs = filter isDigit xs

{-
    The third pattern is called 'fold'; it reduces a list to a single
    value. The following Prelude functions 'fold' (reduce) a list
    to a single result:
    
        sum :: [Int] -> Int
        sum [] = 0
        sum (x:xs) = x + sum xs

        product :: [Int] -> Int
        product [] = 1
        product (x:xs) = x * product xs
        
        concat :: [[a]] -> [a]
        concat [] = []
        concat (xs:xss) = xs ++ concat xss
        
    The operation being called to reduce the list changes:
        +
        *
        ++
        
    And the 'starting' value changes: 0, 1, []
        
    The operators are all 'binary' (each takes 2 arguments)

-}
ex1 = concat [[1,2,3], [4,5,6]]
ex2 = concat ["con","cat","en","ate"]

{-
    *Main> ex1
    [1,2,3,4,5,6]
    *Main> ex2
    "concatenate"

-}
{-
    The fold pattern is generalized in the Prelude function 'foldr'
    
        foldr :: (a -> a -> a) -> a -> [a] -> a
        foldr f a [] = a
        foldr f a (x:xs) = f x (foldr f a xs)    
    
    Type signature:
        (a -> a -> a)   function takes two arguments and returns a value
        a               starting value
        [a]             list
        a               return value
        
    Sum, product and concat can be rewritten using foldr
    
        sum :: [Int] -> Int
        sum xs = foldr (+) 0 xs
        
        product :: [Int] -> Int
        product xs = foldr (*) 1 xs
        
        concat :: [[a]] -> [a]
        concat xs = foldr (++) [] xs    
        
    Walkthrough
    
        sum [1,2]
        ==> foldr (+) 0 [1,2]
        ==> foldr (+) 0 (1 : (2 []))
        ==> 1 + (foldr (+) 0 (2 []))
        ==> 1 + (2 + (foldr (+) 0 []))
        ==> 1 + (2 + 0)
        ==> 1 + 2
        ==> 3
        
    Basically, the function supplied to 'foldr' replaces the list cons
    operator (:) and the 'starting value' replaces the empty list ([])
    
        List (1 :  (2 :  (3 :  [] )) 
        + 0  (1 +  (2 +  (3 +  0) ))
        * 1  (1 *  (2 *  (3 *  1) ))
       ++ [] (a ++ (b ++ (c ++ [] ))
        f z  (a `f`(b `f`(c `f`z  ))  with 'f' as 'infix' operator
        
    Remember that ANY function can be used as an infix operator,
    just surround it with backticks 
    i.e. `div` `odd` `even` `myFunc` etc
        
    'foldr' is 'fold right' - the parentheses accumulate to the right
    of the list, we get
        1 + (2 + (3 + (4 + ( ... ))))
        
    Sometimes called a 'natural fold' because it mimics the way lists
    are built.
    
    [Note: 
       see https://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 ]
    
    There is another version called 'foldl' that is 'left-associative'
    i.e. parentheses accumulate to the left of the list:
        (((0 + 1) + 2) + 3) + ...
        
    As with 'foldr', the function passed in replaces the list (:)
    operator and the starting (zero_ value replaces the empty list []
    but the assumption is that the list STARTS with the empty list
    
        List    []   : 1   : 2   : 3
        + 0   (((0   + 1)  + 2)  + 3)
        * 1   (((1   * 1)  * 2)  * 3)
        ++ [] ((([] ++ a) ++ b) ++ c)
        f z   (((x  `f`a) `f`b) `f`c)
        
    This can cause space (memory) problems when working with infinite 
    lists as evaluation does not begin until the actual end of the list
    is reached; which is never with infinite lists, so you end up
    with stack errors (stack memory becomes full). For this reason,
    always use foldl' (a Prelude foldl which forces evaluation at
    each step to avoid stack errors) assuming you need a foldl

-}
{-
    Putting it all together (creating/using high-order functions)
    
    [Note: Evaluation is from the innermost function to the outermost
           i.e. in fSumSquares, order of evaluation is
                (positives xs) -> squares -> sum
                
                the result of (positives xs) is fed to 'squares'
                and the result of 'squares' is fed to 'sum'
    
-}
fSumSquares xs = sum(squares (positives xs))

-- is the same as 
fSSqrs :: [Int] -> Int
fSSqrs xs = foldr (+) 0 (map sqr (filter pos xs))
    where
    sqr x = x * x
    pos x = x > 0
    
prop_sumSqrs :: [Int] -> Bool
prop_sumSqrs xs = fSumSquares xs == fSSqrs xs

{-
    CURRYING
    
        Explains why type signatures are written as they are
        
        Example
            add :: Int -> Int -> Int
            
            Haskell implicitly associates the above as
                Int -> (Int -> Int)
                
            which corresponds, in the 'add' function below as
                (add x) y = x + y
                
            ie 'add' is a function that takes one argument and
                becomes another function, (add x) that now adds 
                the 'x' argument to something else, so
                    add 3 4 -> (add 3) 4 -> 3 + 4 -> 7
                    
             which is why we get the type signature we do,
                add 3 4 becomes the function (add 3) 4
            AND the VALUE of (add 3) 4 is the final result, 7
            
        Another way to write 'add' is:
        
            add :: Int -> (Int -> Int)
            add x = g                   -- g is a function
                where
                    g y = x + y         -- value of g
                    
        All functions in Haskell work this way; the idea is called
        'currying' 
        
        The key idea is that of reducing a function that takes
        multiple arguments into a series of functions that take
        one argument. So 'add', which takes two arguments, becomes
        a function (add x) which takes a single argument 'y'
        
        You can 'partially' apply any function that takes more than
        one argument by supplying one or more arguments and giving
        the new function a 'name' (see add3 below)
                                                                   
-}
add :: Int -> Int -> Int
add x y = x + y

add3 :: Int -> Int
add3 = add 3                -- partial application

{-
    Example Output:
    
        *Main> add3 1
        4
        *Main> add3 2
        5
        *Main> add3 3
        6    
-}
{-
    Because ALL functions in Haskell are automatically curried
    we can leave off the arguments in our function definitions
    
    For example
        sum :: [Int] -> Int
        sum = foldr (+) 0
        
        is the same as
        
        sum :: [Int] -> Int
        sum xs = foldr (+) 0 xs
        
    The compiler implicitly inserts the 'xs' in the first definition,
    it knows, from the context, that a list is expected. This method
    of defining functions is called 'point free style' in the sense that
    the focus is on the function itself and not as a function
    defined for a given 'point' (ie value)
    
    Definition from Haskell Wiki:
       (https://www.haskell.org/haskellwiki/Pointfree)
       
        "The term originated in topology, a branch of mathematics 
         which works with spaces composed of points, and functions 
         between those spaces. So a 'points-free' definition of a 
         function is one which does not explicitly mention the points 
         (values) of the space on which the function acts. In Haskell, 
         our 'space' is some type, and 'points' are values."
    
    In the sum example above, 'xs' is a list of arbitrary points (values)
    from the 'Int' space (of the Int type) ie. it can be a list of any
    number from [1..n].
    
    [Note:
    
        Remember that functions in Haskell are 'first class entities'
        they are 'values' in and of themselves
        
        From the wiki:
            https://www.haskell.org/haskellwiki/Function
        -------------
        Haskell functions are first class entities, which means that they

            can be given names
            can be the value of some expression
            can be members of a list
            can be elements of a tuple
            can be passed as parameters to a function
            can be returned from a function as a result 
        ------------
    So,
        When we write a function definition in 'point free style' we are 
        emphasizing the 'value' of the function as an entity rather than 
        the arbitrary values it will work on.
        
        i.e. the 'space' the function will apply to (the 'type' of
             values it works with) is more important than any 
             arbitrary value from that space.
             
        In a sense, a 'point free' definition is a 'generalized'
        definition; our focus is not on any specific value the
        function will be applied to but the entire 'type class' of
        values it will be applied to.      
        
    ]
-}


