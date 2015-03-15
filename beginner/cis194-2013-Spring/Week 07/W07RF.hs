module W07RF where

{-
    Week 07 - Folds and Monoids
              Notes from Suggested Readings for Folds
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-} 
{-
    Ref: Learn You a Haskell - Only folds and horses
         http://learnyouahaskell.com/higher-order-functions#folds
         
    ---------------------
    You can use a fold whenever you want to traverse a list once and 
    return something.

    A 'fold' reduces a list to single value using a binary function and
    a starting value. A binary function takes two values and is called
    with the starting value (which becomes the accumulator) and the 
    first (or last) value in the list, returning a new accumulator
    (starting value) which becomes an argument to the next function call.
    
    'foldl' folds up a list from the left with the binary function
    being applied first to the starting value and head of the list.
    [Note: if the list is empty, the starting value is returned.]
    
    Example (see sum' function below):
    
            sum' [3,5,2,1]
         -> foldl (\acc x -> acc + x)  0 [3,5,2,1]
         -> foldl (\0 3   -> 0   + 3)  0 [5,2,1]
         -> foldl (\acc x -> acc + x)  3 [5,2,1]
         -> foldl (\3 5   -> 3   + 5)  3 [2,1]
         -> foldl (\acc x -> acc + x)  8 [2,1]
         -> foldl (\8 2   -> 8   + 2)  8 [1]
         -> foldl (\acc x -> acc + x) 10 [1]
         -> foldl (\10 1  -> 10  + 1) 10 []
         -> foldl (\acc x -> acc + x) 11 []
         -> 11
         
    [Note: the accumulator is not evaluated at each step, instead
           a chain of values is built and then reduced at the end;
           which is why a left fold can cause stack overflows with
           large lists. The above is closer to what happens when
           the Prelude function foldl' is used]
         
    The function can be written using a 'section' and point-free
    style:
    
        sum' = foldl (+) 0
        
    The section (+) implies the lambda function (\acc x -> acc + x)
    used above.
    
    Another function that can be implemented with a left fold is
    'elem' (see below). Note that the starting value in this case
    is a boolean value, False.  The returned value and the starting
    value must ALWAYS be of the same type.
    
    The right fold, foldr, works in a similar fashion but starts
    from the right and the order of the accumulators arguments is
    reversed i.e. (\x acc -> ...) vs (\acc x -> ...).
    
    Example:
            map' (+3) [1,2,3]
         -> foldr (\x acc     -> f x   : acc)   []      [1,2,3]
         -> foldr (\3 []      -> 3 + 3 : [] )   []      [1,2]
         -> foldr (\x acc     -> f x   : acc)   [6]     [1,2]
         -> foldr (\2 [6]     -> 2 + 3 : [6])   [6]     [1]
         -> foldr (\x acc     -> f x   : acc)   [5,6]   [1]
         -> foldr (\1 [5,6]   -> 1 + 3 : [5,6]) [5,6]   []
         -> foldr (\x acc     -> f x   : acc  ) [4,5,6] []
         -> [4,5,6]
    
    [Note: again, the above is only a rough depiction of what happens.]
    
    map' could also be implemented using a left fold:
    
        map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
        
    but the (++) operator is much more expensive than the (:) operator.
    
    Right folds will always work on infinite lists as you start at some 
    point on the right and fold to the beginning and all lists have a 
    beginning.
    
    Left fold will not work on infinite lists as you start at the
    beginning and try to fold to an end which is never reached.
    
    The Prelude also has definitions for foldr1 and foldl1 which
    are the same as foldr and foldl but don't take a start value;
    instead, they assume the last or first element is the starting
    value. So, we could have implemented sum as:
    
        sum' = foldr (+)
        
    Note that these functions will cause runtime errors if the
    list is empty.
    
    Another way of picturing folds
    ------------------------------
    If we have a right fold, a function f, a starting value z and
    the list [3,4,5,6] then we are essentially doing this:
    
            3 : 4 : 5 : 6 : []
       -> f 3(f 4(f 5(f 6    z)))
       
    i.e. we are replacing the (:) list operator with 'f' and the
         empty list operator, [], with 'z'.
    
    If f is (+) and the starting value is 0 then
        
            3 : 4 : 5 : 6 : []
         -> 3(+ 4(+ 5(+ 6(+ 0))))
         -> 3(+ 4(+ 5(+ 6 + 0)))
         -> 3(+ 4(+ 5 + 6))
         -> 3(+ 4 + 11)
         -> 3 + 15
         -> 18
            
    [Note: the parentheses accumulate to the right]
    
    If we have a left fold with a function g, an accumulator z and
    the list [3,4,5,6] then we are essentially doing:
    
            g (g (g (g z 3) 4) 5) 6
            
    If we use (flip (:)) as the function and [] as z then:
    
        flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6
     -> flip (:) (flip (:) (flip (:) (3 : []) 4) 5) 6
     -> flip (:) (flip (:) (4 : 3 : []) 5) 6     
     -> flip (:) (5 : 4 : 3 : []) 6
     -> 6 : 5 : 4 : 3 : []
     -> [6,5,4,3]
    
    scanl and scanr
    ---------------
    scanl and scanr work like foldl and foldr but they return
    all the intermediate steps.  With scanl, the final result
    will be in the last element of the result list. With scanr,
    the final result will be in the first element of the result
    list.
    
    ghci> scanl (+) 0 [3,5,2,1]  
    [0,3,8,10,11]  
    ghci> scanr (+) 0 [3,5,2,1]  
    [11,8,3,1,0]  
    ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
    [3,4,5,5,7,9,9,9]  
    ghci> scanl (flip (:)) [] [3,2,1]  
    [[],[3],[2,3],[1,2,3]]  

    Use a scan when you need to monitor the progress of a fold (see
    sqrtSums example below)

-}
-- standard Prelude functions implemented as folds
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

-- how many elements does it take for the sum of the roots of all
-- natural numbers to exceed 1000?
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1 

{-
    Ref: Haskell Wiki - Fold
         https://wiki.haskell.org/Fold
 
    'fold' (or reduce) is a family of higher order functions that process 
    a data structure and build a return value.
    
    [NOTE: there is no 'fold' function, you have to use foldr or foldl]
    
    A fold usually begins with a combining function and a data structure
    whose elements are to be combined using the function. For example,
            fold (+) [1,2,3,4,5] -> 15
            
    (+) is an associative operator so it doesn't matter how the list
    is parenthesized; visually, you can imagine the (+) replacing
    the commas in the list.  However, not every function is associative
    and so the order of parentheses matters i.e.  4/2 is not the same
    as 2/4.  
    
    There are two ways to fold a list, from the left, where we begin
    with the first element or from the right, where we begin with
    the last element.  It is also natural to have a starting value,
    which, when folding from the left, we use as the first value and
    when folding from the right, we use as the last value. This is
    easier to see from the following fold definitions:
    
    -- if the list is empty, the result is the initial value z; else
    -- apply f to the first element and the result of folding the rest
    foldr f z []     = z 
    foldr f z (x:xs) = f x (foldr f z xs) 
     
    -- if the list is empty, the result is the initial value; else
    -- we recurse immediately, making the new initial value the result
    -- of combining the old initial value with the first element.
    foldl f z []     = z                  
    foldl f z (x:xs) = foldl f (f z x) xs
    
    Note that due to 'lazy evaluation', a right fold will always
    terminate (f x is always fully evaluated); it can return a value
    without fully evaluating the recursive case; which is not true
    of a left fold; if an infinite list is used there is no end
    point that can be reached with the recursive case, it will just
    keep on churning through the the elements.  If you want to do a
    left fold on an infinite list, use foldl'
    
    If you want to do a fold over a list with no readily available
    starting value ie finding the maximum number in a list, use
    foldr1 or foldl1 which use the last or first element of the
    list, respectively, as their starting value.
    
    Tree-like folds
    ---------------
    A starting value is mandatory when the desired result is of
    a different type than the elements of the list (function is
    'asymmetrical' in its types). The starting value must have the 
    same type as the function result.
    
    When a function is symmetrical in its types (the type of the
    function result is the same as the type of the list elements)
    we don't much care how the expression chain is parenthesized.
    
    Both finite and infinite lists can be can be folded in a tree
    like fashion.
    
    List folds as structural transformations
    ----------------------------------------
    One way to view folds is as a mechanism for replacing structural
    components in a data structure. For example, lists are built with
    the cons operator (:) and the nil operator, [], and we can 
    imagine both left and right folds replacing the structural components
    with a function and initial value. For example,
    
    [1,2,3,4,5]     foldr f z                 foldl f z
    
       :                  f                         f     
      / \                / \                       / \
     1   :              1   f                     f   5
        / \                / \                   / \ 
       2   :              2   f                 f    4
          / \                / \               / \
         3   :              3   f             f   3
            / \                / \           / \ 
           4   :              4   f         f   2
              / \                / \       / \
             5   []             5   z     z   1
             
    This makes it obvious that foldr (:) [] is the identity function
    for lists.
    
    The diagrams also make it easy to see that foldl (flip (:)) []
    is an easy way to reverse a list (the args to flip are reversed).
    
    They also make it easy to see a simple way to write map, so that
    a function will work with the (:) operator
    
        map f = foldr ((:) . f) []
        
    i.e. f will be applied to each element before the (:) operator
    
    Looking at folds this way also makes it easier to visualize
    how folds can be used with other data types; we write a function
    that can recursively replace the constructors of the data type
    we're working with and any constant values that are part of 
    they type.
    
    We can use ghci to visualize fold structure transformations;
    see the examples below.
    
-}
-- examples of folds as tree like structures
foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
 
foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
 
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t

-- visualize struture transforms
ex1 = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
ex2 = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
ex3 = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
ex4 = foldi (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])

-- infinite tree-like folding can be seen with the Sieve of Eratoshtenes
primes :: (Integral a) => [a]
primes = 2 : 3 : ([5,7..] `minus`
                     foldi (\(x:xs) -> (x:) . union xs) []
                          [[p*p, p*p+2*p..] | p <- tail primes])
    where    
        -- ordered lists, difference and union
        minus (x:xs) (y:ys) = case (compare x y) of 
                   LT -> x : minus  xs  (y:ys)
                   EQ ->     minus  xs     ys 
                   GT ->     minus (x:xs)  ys
        minus  xs     _     = xs
        
        union (x:xs) (y:ys) = case (compare x y) of 
                   LT -> x : union  xs  (y:ys)
                   EQ -> x : union  xs     ys 
                   GT -> y : union (x:xs)  ys
        union  xs     []    = xs
        union  []     ys    = ys    
                          
-- a merge sort for finite lists defined as a fold
mergesort    :: (Ord a) => [a] -> [a]
mergesort xs = foldt merge [] [[x] | x <- xs]
    where
        merge (x:xs) (y:ys) = case (compare x y) of 
                   LT -> x : merge  xs  (y:ys)
                   EQ -> x : merge  xs  (y:ys)
                   GT -> y : merge (x:xs)  ys
        merge  xs     []     = xs
        merge  []     ys     = ys    
    