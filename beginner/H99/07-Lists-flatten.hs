{-

    Problem 7
    
    (**) Flatten a nested list structure.

    Transform a list, possibly holding lists as elements into a `flat' 
    list by replacing each list with its elements (recursively).

    Example:

    * (my-flatten '(a (b (c d) e)))
    (A B C D E)

    Example in Haskell:

    We have to define a new data type, because lists in Haskell are 
    homogeneous. [Note: [1,[2,[3,4],5]] would be illegal ]

    data NestedList a = Elem a | List [NestedList a]

    *Main> flatten (Elem 5)
    [5]
    *Main> flatten (List [Elem 1, 
                          List [Elem 2, List [Elem 3, Elem 4], 
                          Elem 5]])
    [1,2,3,4,5]
    *Main> flatten (List [])
    []    

-}
data NestedList a = Elem a
                  | List [NestedList a]
        deriving Show
                  
flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List [] )    = []      
flatten (List (a:as)) = flatten a ++ flatten (List as)

-- other methods from H99
-- concatMap - maps a function to a list and concatenates results
flatten_a :: NestedList a -> [a]
flatten_a (Elem x) = [x]
flatten_a (List x) = concatMap flatten x

-- acts like concatMap
flatten_b (Elem x) = return x
flatten_b (List x) = flatten =<< x      -- bind with args reversed

flatten_c :: NestedList a -> [a]
flatten_c (Elem x ) = [x]
flatten_c (List xs) =  foldr (++) [] $ map flatten_c xs

flatten_d = reverse . rec []
  where
      rec acc (List []) = acc
      rec acc (Elem x)  = x:acc
      rec acc (List (x:xs)) = rec (rec acc x) (List xs)

-- tests -----------------------------------------------------------------
lsta = (Elem 5)
lstb = (List [Elem 1,
              List [Elem 2, List [Elem 3, Elem 4], Elem 5]])      
lstc = (List [])             

testNL f = f lsta == [5] && f lstb == [1,2,3,4,5] && f lstc == []

tests = testNL flatten   && testNL flatten_a && testNL flatten_b
     && testNL flatten_c && testNL flatten_d
     
     