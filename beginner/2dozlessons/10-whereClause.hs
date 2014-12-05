-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 10 - Private Definitions: The where clause

{-
    The 'where-clause' allows us to define terms that will be 
    visible only within the context of the enclosing definition
    
    A where-clause appears as an indented subsection of a higher-level 
    definition. The indentation is Haskell’s way of marking subsections — 
    it is a bracketing method known as the 'offsides rule'. A where-clause 
    may contain any number of private definitions. The end of a 
    where-clause occurs when the level of indentation moves back to the 
    left of the level established by the keyword where that begins the 
    where clause
    
    (this 'hiding' of functions/declarations from other areas
     of code is called 'encapsulation')
    
-}
{-
   compute the value of a number (whose digits are given in 
   reverse order) using the Horner Formula:
   
        d0 + 10 x (d1 + ... + 10 x (dn-1 + 10xdn)...)
 
-}
horner10 ds = foldr1 multAdd ds
    where
        multAdd d s = d + 10*s
        
{-
   The 'multAdd' function is visible only to the 'horner10' function
   
   Example:
        *Main> horner10 [1,2,3,4]
        4321
        *Main> multAdd 2 1
        <interactive>:64:1: Not in scope: `multAdd'
        *Main>    
    
    horner10 [1,2,3,4]  is evaluated as:
    ==> foldr1 multAdd ds
    ==> 1 multAdd 2 multAdd 3 multAdd 4
    ==> 1 multAdd 2 multAdd (3 multAdd 4)
    ==> 1 multAdd 2 multAdd ( 3 + 10 x 4)
    ==> 1 multAdd (2 multAdd 43)
    ==> 1 multAdd (2 + 10 x 43)
    ==> 1 multAdd 432
    ==> 1 + 10 x 432
    ==> 4321
    
    The above is a bit quirky as you have to list the digits
    of the number you want translated in reverse order. The
    following function takes the digits of a decimal number
    in their normal order and returns their decimal value.
    
-}
integerFromDecimalNumber :: Num a => [a] -> a        
integerFromDecimalNumber ds = (horner10 . reverse) ds
        
{-
    The above works ok when given a valid decimal number or
    even a number with leading zero's:
    
        *Main> integerFromDecimalNumber [1,4,9,2]
        1492
        *Main> integerFromDecimalNumber [0,0,1,4,9,2]
        1492
        
    but what if is given an empty list?
    
        *Main> integerFromDecimalNumber []
        *** Exception: Prelude.foldr1: empty list    
    
    Turns out Haskell has another built-in function, 'foldr',
    that takes 3 arguments rather than 2.
    
        foldr op z xs
        
    where, op = function to be applied to list elements
           z  = zero or starting value or accumulator
           xs = list to be folded
           
           the 'z' value acts as a container to hold the
           accumulating results of the function application
           to each element in the list so often referred to
           as 'the accumulatro'
           
    we can revise our original function to use foldr rather
    than foldr1; we just need to decide what are starting value
    is. In this case, if the list is empty then we want to 
    return zero.
-}
horner10' :: Num a => [a] -> a
horner10' ds =
    foldr multAdd 0 ds
    where
        multAdd d s = d + 10*s

integerFromDecimalNumber' :: Num a => [a] -> a
integerFromDecimalNumber' ds = (horner10' . reverse) ds        
{-
    Now instead of getting an error on an empty list we get:
    
        *Main> integerFromDecimalNumber' []
        0    
        
    Note that the 'multAdd' declared in this where-clause does not
    clash with the earlier definition of multAdd; the two are in
    completely separate namespaces.
    
    Also note that in live code we would just modify the orginal
    horner10 function.
    
    We can write a similar function that will total the numbers
    in a list.
-}
total :: Num a => [a] -> a
total xs = foldr (+) 0 xs

{-
    Example:
    
        total [12,3,5,1,4]
        ==> 12 + (3 + (5 + (1 + (4 + 0)))
        ==> 12 + (3 + (5 + (1 + 4)))
        ==> 12 + (3 + (5 + 5))
        ==> 12 + (3 + 10)
        ==> 12 + 13
        ==> 25
        
-}
-- given the function
ct :: Num a => [a] -> a
ct xs = foldr addOne 0 xs
    where
        addOne x sum = 1 + sum
        
-- what is the result of ct [1,2,3,4]? 10, 4, 5, or an error?
res1 = ct [1,2,3,4]

        
