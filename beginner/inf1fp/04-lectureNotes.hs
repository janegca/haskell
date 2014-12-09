-- Informatics 1 - Functional Programming 
-- Lecture 4 - Lists and Recursions
--      
--
{-
    List Operators
    
    (:)  Cons     - appends an element to a list ('Cons' for 'construct')
    (++) Append   - appends one list to another
    
    Type signatures
        (:)  ::  a  -> [a] -> [a]
        (++) :: [a] -> [a] -> [a]
        
        where 'a' is a TYPE VARIABLE and indicates 'any type of element' 
        will work; caveat is the type of the first
        
    Cons and Append are both 'infix' operators; if you want to use them
    as 'prefix' operations, you must surround them in parentheses
-}
ex1 = 1 : [2,3]         -- Cons as infix operator
ex1a = (:) 1 [2,3]      -- Cons as prefix operator
ex2 = [1] ++ [2,3]      -- Append as infix operator
ex2a = (++) [1] [2,3]   -- Append as prefix operator
ex3 = [1,2] ++ [3]
ex4 = 'l' : "ist"
ex5 = "l" ++ "ist"
ex6 = "li" ++ "st"

{-
    Example Output:
    
        *Main> ex1
        [1,2,3]
        *Main> ex1a
        [1,2,3]

        *Main> ex2
        [1,2,3]
        *Main> ex2a
        [1,2,3]

        *Main> ex3
        [1,2,3]
        *Main> ex4
        "list"
        *Main> ex5
        "list"
        *Main> ex5
        "list"
    
-}
{-
    Cons
        Every list can be written using Cons (:) and Nil [] (empty list)

-}
ex8 = 1 : (2 : (3 : []))
ex9 = 'l' :('i' : ('s' : ('t' : [])))

{-
    Example Output:
    
        *Main> ex8
        [1,2,3]
        *Main> ex9
        "list"
    
-}
{-
    A recursive definition: A list is either
        empty       - written as [], or
        constructed - write x:xs with
                        head x    the first (left most) list element
                        tail xs   the rest of the list as a list  

    (the definition is self-referential,  it defines itself in terms
     of itself i.e. a list is built by tacking a list element onto
     a list)
-}
ex10  = null [1,2,3]            -- check for empty list
ex10a = null []
ex11  = head [1,2,3]            -- get first element
ex11a = head []
ex12  = tail [1,2,3]            -- get all but first element
ex12a = tail []

{-
    Example Output:
    
        *Main> ex10
        False
        *Main> ex10a
        True
        *Main> ex11
        1
        *Main> ex11a
        *** Exception: Prelude.head: empty list
        *Main> ex12
        [2,3]
        *Main> ex12a
        *** Exception: Prelude.tail: empty list

-}
{-
    List Comprehensions and Recursion
    
    You can write a function over a list as a comprehension
    or as a recursive definition.
    
    The recursive definition has two cases, one for the empty
    list, [], and one for the constructed list, (x:xs)
    This method of defining 'cases' is called 'pattern matching'
    
-}
squares :: [Integer] -> [Integer]
squares xs = [ x*x | x <- xs ]

squaresRec :: [Integer] -> [Integer]
squaresRec []     = []
squaresRec (x:xs) = x*x : squaresRec xs     -- self-referential

{-
    Example Output:
    
    *Main> squares [1,2,3]
    [1,4,9]
    *Main> squaresRec [1,2,3]
    [1,4,9]
    
    Walkthrough of example
        squaresRec [1,2,3]
        ==> squaresRec (1 : (2 : 3 : [])))
        ==> 1*1 : squaresRec (2 : (3 : []))     -- x=1, xs=[2,3]
        ==> 1*1 :(2*2 : squaresRec (3 : []))    -- x=2, xs=[3]
        ==> 1*1 : (2*2 : (3*3 : []))            -- x=3, xs=[]
        ==> 1 : (4 : (9 : [])                   -- do the math
        ==> [1,4,9]  
-}
{-
    Could also define squares using conditionals
    This resembles the way it might be written in an imperative
    language (Java, C, Python, etc)
-}
squaresCond :: [Integer] -> [Integer]
squaresCond ws =
    if null ws then
        []
    else
        let x = head ws             -- local variables
            xs = tail ws
        in
            x*x : squaresCond xs

{-
    Example Output:
        *Main> squaresCond [1,2,3]
        [1,4,9]    
-}
