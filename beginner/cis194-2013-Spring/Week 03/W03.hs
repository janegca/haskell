module W03 where

{-
    Week 03 - Recursion Patterns, Polymorphism, the Prelude
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/
        lectures/03-rec-poly.html
-}

{-
    Recursion Patterns
    ------------------
    
    Things we might do with an IntList
        map    - perform some operation on the list elements
        filter - keep some elements, discard others
        fold    - summarize the elements (to be covered in Week 4)
    
-}
data IntList = Empty | Cons Int IntList
  deriving (Eq, Show)
  
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))
  
-- MAP
-- add one to every element
plusOne :: IntList -> IntList
plusOne Empty       = Empty
plusOne (Cons x xs) = Cons (x + 1) (plusOne xs)
  
-- ensure all elements are positive  
absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)  

-- square every element
squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

-- all the above follow the same pattern which can be abstracted as
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

-- we can now rewrite our functions as simple, non-recursive, functions
addOne x = x + 1
square x = x * x

map1 = plusOne   exampleList == mapIntList addOne exampleList
map2 = absAll    exampleList == mapIntList abs    exampleList
map3 = squareAll exampleList == mapIntList square exampleList

mapTest = map1 && map2 && map3

-- FILTER
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x    = Cons x (keepOnlyEven xs)
    | otherwise = keepOnlyEven xs
  
-- again, we can abstract a pattern
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty 
filterIntList f (Cons x xs) 
    | f x       = Cons x (filterIntList f xs)
    | otherwise = filterIntList f xs
                               
evenNum n = mod n 2 == 0
                               
filterTest = keepOnlyEven exampleList == filterIntList evenNum exampleList

{-
    Polymorphism
    ------------
    Haskell supports polymorphism for both data types and functions.
    
    List (below) is a polymorphic data type
        the 't' stands for any 'type'
        data List t = ... means 'List' is 'parameterized' by a type
        
    Functions can also be made polymorphic (see below)
    
    Note: the 'type' variables (t, a, b, ...) must be lowercase
          polymorphic functions MUST work for every possible type
          
    The 'Prelude.hs' module, imported automatically into every
    Haskell program, has many polymorphic functions as well as
    a polymorphic definition of lists.
    
-}
-- polymorphic data type
data List t = E | C t (List t) deriving (Eq, Show)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

-- polymorphic functions
filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
    | p x       = C x (filterList p xs)
    | otherwise = filterList p xs
    
mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)    
                             

exF1 = filterList (> 2) lst1
exM1 = mapList (* 2) lst1
  
{-
    Total and Partial Functions
    ---------------------------
    The 'head' function, for grabbing the first element in a list,
    is a 'partial' function; it will crash if the list is empty.
    
    A function that will recurse forever is also a 'partial' function.
    
    'Well defined' functions, i.e. functions that work for every
    possible input, are called 'total' functions.
    
    Partial functions should be avoided; they are mistakes. 
    Other partial functions in the Prelude are: tail, init, last and !!.
    
    So, what are we to do if we are to avoid these functions?
    Usually we can replace them with functions that use pattern matching.
    The other option is to write 'safe' methods using Maybe.
    
    The example, safeHead, is, in some sense, still partial but the
    partiality is explicit in the definition.
-}  
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- if a condition is guaranteed it should be reflected in the types
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as