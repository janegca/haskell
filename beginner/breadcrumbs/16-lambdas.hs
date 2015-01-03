-- Lambdas
-- Note: the term 'lambda' is a Greek letter and it's use
--       in programming comes from Alonzo Church's work
--       'lambda calculus'
--
-- we can define functions on the fly with lambdas
-- their use will make more sense later on


foo f x y = f x y
callFoo = foo (\a b -> a + b) 1 2 
-- we define an anonymous function [one without a name] that 
-- takes arguments a and b, and adds them together
-- and we call it right away with arguments 1 and 2


-- a good example uses map from the standard library, which
-- applies a function to everything in a list
addOneToEverything lst = map (\a -> a + 1) lst

{-
    Example output:
    
    *Main> callFoo
    3
    *Main> addOneToEverything [1..10]
    [2,3,4,5,6,7,8,9,10,11]
    *Main>     
    
    Note: a better way to do the map would be to use a section
          don't get carried away with lambda's if there is a
          cleaner solution
    
    *Main> map (+1) [1..10]
    [2,3,4,5,6,7,8,9,10,11]
    
-}
