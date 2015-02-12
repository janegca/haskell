-- Chapter 11 - Functions - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm

{-
    Exercise 1
    
    Decide whether the following functions are partial or total, and
    then run the tests on the computer:
    
        (a) isPartialFunction
            [1,2,3] [2,3]
            [(1,Value 2),(2,Value 3),(3,Undefined)]     -- Yes
        
        (b) isPartialFunction
            [1,2] [2,3]
            [(1,Value 2),(2,Value 3)]                   -- No
-}

ex1a = isPartialFunction [1,2,3] [2,3]
                         [(1,Value 2),(2,Value 3),(3,Undefined)]
                         
ex1b = isPartialFunction [1,2] [2,3] [(1,Value 2),(2,Value 3)]                         

{-
    Exercise 2
    
    Work out the following expressions, by hand and using the computer:
    (i.e. are they functions? yes if the domain and function domain
          are the same)

        isFun [1,2,3] [1,2] [(1,Value 2),(2,Value 2)]  
            domain          [1,2,3]
            function domain [1,2]
            so, no, not a function
        
        isFun [1,2,3] [1,2] [(1,Value 2),(2,Value 2),
                             (3,Value 2),(3,Value 1)] 
            domain          [1,2,3]
            function domain [1,2,3,3]
            so, no, not a function
                             
        isFun [1,2,3] [1,2] [(1,Value 2), (2,Value 2),(3,Value 2)] 
            domain          [1,2,3]
            function domain [1,2,3]
            so yes, a function

-}
ex2a = isFun [1,2,3] [1,2] [(1,Value 2),(2,Value 2)]  -- False
ex2b = isFun [1,2,3] [1,2] [(1,Value 2),(2,Value 2),
                            (3,Value 2),(3,Value 1)]  -- False
ex2c = isFun [1,2,3] [1,2] [(1,Value 2), (2,Value 2),(3,Value 2)] -- True

{-
    Exercise 3
    
    What is the value of mystery x where mystery is defined as:

        mystery :: Int -> Int
        mystery x = if mystery x == 2 then 1 else 3

    Ans: never terminates so value is 'bottom' (undefined)
-}    
mystery :: Int -> Int
mystery x = if mystery x == 2 then 1 else 3
                        
ex3a = mystery 2        -- stack overflow
ex3b = mystery 1        -- stack overflow

{-
    Exercise 4
    
    What is the value of mystery2 x where mystery2 is defined as:

        mystery2 :: Int -> Int
        mystery2 x = if x == 20 then 2 + mystery2 x else 3

    Ans: 3 if x is anything other than 20, if x == 20, non-terminating
-}
mystery2 :: Int -> Int
mystery2 x = if x == 20 then 2 + mystery2 x else 3

ex4a = mystery2 5       -- 3
ex4b = mystery2 20      -- stack overflow

{-
    Exercise 5
    
    Work out the values of the following expressions, and then check
    your result by evaluating them with the computer:

        map (increment.increment.increment) [1,2,3]
        
     -> (increment(increment(increment 1) : map (inc.inc.inc) [2,3]
     -> 4 :( (inc(inc(inc 2))) :( map (inc.inc.inc) [3]))
     -> 4 : 5 : (inc(inc(inc 3))) : []
     -> 4 : 5 : 6 : []
     -> [4,5,6]
        
        map ((+ 2).(* 2)) [1,2,3]
     -> ((1*2)+2) :( map((+2).(* 2)) [2,3])
     -> 4 :( (2*2)+2) :( map((+2).(*2)) [3])
     -> 4 : 6 : ((3*2)+2) : []
     -> 4 : 6 : 8 : []
     -> [4,6,8]
-}
increment :: Int -> Int
increment n = n + 1

ex5a = map (increment.increment.increment) [1,2,3]  -- [4,5,6]
ex5b = map ((+ 2).(* 2)) [1,2,3]                    -- [4,6,8]

{-
    Exercise 6
    
    Using the definitions below, work out the type and graph of f.g,
    and check using the computer.

        f : Int -> String
        f 1 = "cat"
        f 2 = "dog"
        f 3 = "mouse"
        
        g : Char -> Int
        g 'a' = 1
        g 'b' = 2
        g 'c' = 2
        g 'd' = 3        
        
    Ans:
        (f.g) :: (Int -> String) -> (Char -> Int) -> Char 
        f . g = f( g )
                
        f( g 'a') = f(1) = "cat"
        f( g 'b') = f(2) = "dog"
        f( g 'c') = f(2) = "dog"
        f( g 'd') = f(3) = "mouse"
        
        (['a','b','c','d'], [('a',Value 1), ('b',Value 2), ('c', Value 3)])
        ([1,2,3], [(1,Value "cat"),(2,Value "dog"),(3,Value "mouse")])
        (['a','b','c','d'], [('a',Value "cat"),('b',Value "dog"),
                             ('c',Value "dog"),('d',Value "mouse")])
-}
f :: Int -> String
f 1 = "cat"
f 2 = "dog"
f 3 = "mouse"

g :: Char -> Int
g 'a' = 1
g 'b' = 2
g 'c' = 2
g 'd' = 3        

ex6 = functionalComposition 
        [('a',Value 1), ('b',Value 2), ('c', Value 2), ('d', Value 3)]
        [(1,Value "cat"),(2,Value "dog"),(3,Value "mouse")]

{-
    Output:
    
        Main> ex6
        [('a',Value "cat"),('b',Value "dog"),('c',Value "dog"),
         ('d',Value "mouse")]
        Main>     
-}
{-
    Exercise 7
    
    Functions are often composed with each other in order to form a
    pipeline that processes some data. What does the following expression
    do?

        ((map (+ 1)).(map snd)) xs
        
    Ans:
        Grabs the second element in a list of pairs
        and increments it by 1; returning a list of just the
        incremented elements.
-}
ex7 = ((map (+1)) . (map snd)) [(1,2),(1,2),(1,2)]  -- [3,3,3]

{-
    Exercise 8
    
    Sometimes access to deeply nested constructor expressions is performed
    by function composition. What is the value of this expression?

        (fst.snd.fst) ((1,(2,3)),4)
        
    Ans: 
        (fst.snd.fst) ((1, (2,3)), 4)
     -> fst(snd((1, (2,3))))
     -> fst( (2,3) )
     -> 2
-}
ex8 = (fst.snd.fst) ((1,(2,3)),4)       -- 2

--
-- Properties of Functions 
--
{-
    Exercise 9

    Decide whether the functions represented by the graphs in the
    following examples are surjective, and then check using the computer:

    isSurjective [1,2,3] [4,5]
                 [(1, Value 4), (2, Value 5), (3, Value 4)]
                 
        -- Yes. Image contains all values in result type
                 
    isSurjective [1,2,3] [4,5]
                 [(1, Value 4), (2, Value 4), (3, Value 4)] 

        -- No. Image doesn't contain '5' found in the result type

-}
ex9a = isSurjective [1,2,3] [4,5] 
                    [(1, Value 4), (2, Value 5), (3, Value 4)]  -- True
ex9b = isSurjective [1,2,3] [4,5]
                    [(1, Value 4), (2, Value 4), (3, Value 4)]  -- False

{-
    Exercise 10
    
    Which of the following functions are surjective?
    
    (a) f :: A → B, where A = {1, 2}, B = {2, 3, 4} and 
                          f = {(1, 2), (2, 3)}
                          
    (b) g :: C → D, where C = {1, 2, 3}, D = {1, 2} and
                          g = {(1, 1), (2, 1), (3, 2)}    
                          
    Ans: f is not surjective, g is surjective

-}                    
ex10a = isSurjective [1,2] [2,3,4] 
                     [(1, Value 2), (2, Value 3)]   -- False
                     
ex10b = isSurjective [1,2,3] [1,2] 
                     [(1,Value 1),(2, Value 1), (3, Value 2)] -- True
                     
{-
    Exercise 11
    
    Which of the following functions are not surjective, and why?
    
    (a) map increment :: [Int] -> [Int]
            surjective, image includes all Ints
    
    (b) take 0 :: [a] -> [a]
            not surjective, image is empty
    
    (c) drop 0 :: [a] -> [a]
            surjective, image includes all 'a' values
    
    (d) (++) xs :: [a] -> [a]   
            not surjective, image contains one value

-}                
{-
    Exercise 12
    
    Determine whether the functions in these examples are injective,
    and check your conclusions using the computer:
        (a) isInjective [1,2,3] [2,4]
                        [(1,Value 2),(2,Value 4),(3,Value 2)]

        (b) isInjective [1,2,3] [2,3,4]
                        [(1,Value 2),(2,Value 4),(3,Undefined)]    

        Ans: (a) False, '2' is repeated in the image
             (b) True, undefined is a unique value
-}     
ex12a = isInjective [1,2,3] [2,4]
                    [(1,Value 2),(2,Value 4),(3,Value 2)]    -- False
ex12b = isInjective [1,2,3] [2,3,4]
                    [(1,Value 2),(2,Value 4),(3,Undefined)]  -- True

{-
    Exercise 13
    
    Which of the following functions are injective?

    (a) f :: A → B, where A = {1, 2}, B = {1, 2, 3} and 
                          f = {(1, 2), (2, 3)}.
    (b) g :: C → D, where C = {1, 2, 3}, D = {1, 2} and 
                          g = {(1, 1), (2, 2)}.    

    Ans: 'f' is injective, 
         'g' is not injective (doesn't use every 'a')
-}                    
ex13a = isInjective [1,2]   [1,2,3] [(1,Value 2),(2,Value 3)]   -- True
ex13b = isInjective [1,2,3] [1,2] [(1,Value 1),(2,Value 2)]     -- False

{-
    Exercise 14
    
    Suppose that f :: A → B and A has more elements than B.
    Can f be injective? 

    Ans: No. Every element of A has to map to a unique element
         of B. That is not possible if A is larger than B 
         (see ex13b)

-}
--
-- Bijective Functions
--
{-
    Exercise 15
    
    Determine whether the following functions are bijective, and
    check your conclusions using the computer:

    isBijective [1,2] [3,4] [(1,Value 3),(2,Value 4)] -- True
    isBijective [1,2] [3,4] [(1,Value 3),(2,Value 3)] -- False   

-}
ex15a = isBijective [1,2] [3,4] [(1,Value 3),(2,Value 4)]
ex15b = isBijective [1,2] [3,4] [(1,Value 3),(2,Value 3)]

{-
    Exercise 16
    
    Let A = {1, 2, 3} and f :: A → A, where
        f = {(1, 3), (2, 1), (3, 2)}. 
    Is f bijective? Is it a permutation? 

    Ans: 'f' is both bijective and a permutation

-}
ex16a = isBijective [1,2,3] [1,2,3] 
                    [(1,Value 3),(2,Value 1),(3,Value 2)]    -- True
ex16b = isPermutation [1,2,3] [1,2,3] 
                      [(1,Value 3),(2,Value 1),(3,Value 2)]  -- True

{-
    Exercise 17
    
    Determine whether the following functions are permutations,
    and check using the computer:
        isPermutation [1,2,3] [1,2,3]
                      [(1,Value 2),(2, Value 3),(3, Undefined)]
                      
        isPermutation [1,2,3] [1,2,3]
                      [(1,Value 2),(2, Value 3),(3, Value 1)]   

    Ans: first is not a permutation (not one-to-one)
         second is a permutation
-}                      
ex17a = isPermutation [1,2,3] [1,2,3]
                      [(1,Value 2),(2, Value 3),(3, Undefined)] -- False
              
ex17b = isPermutation [1,2,3] [1,2,3]
                      [(1,Value 2),(2, Value 3),(3, Value 1)]   -- True 

{-
    Exercise 18
    
    Is f, defined below, a permutation?
    
        f :: Integer -> Integer
        f x = x+1
        
    Ans: Yes. Image of f would run as [...,(0,1),(1,2),(2,3),...]
         with a unique 'successor' integer for every input integer.
-}                      
{-
    Exercise 19
    
    Suppose we know that the composition f ◦ g of the functions f
    and g is surjective. Show that f is surjective
    
    Ans:
        f :: A -> B
        g :: A -> B
        (f . g) :: A -> B
        
        As (f . g) is the combination of the results of 'f' and 'g'
        it can only be surjective if 'f' and 'g' are surjective
        
        If 'f' contains any ordered pairs (a,b),(a',b) then
        they would also be in the image of (f . g) which would
        mean (f . g) would not be surjective, which is not true.
        
        The same holds for 'g'.
-}
{-
    Exercise 20
    
    Suppose that f :: A -> A is a permutation. What can you say
    about f^(-1)?    
    
    Ans: f^(-1) is also a permutation with the type A -> A
-}
{-
    Exercise 21
    
    Explain why there cannot be a finite set that satisfies Definition
    78.
    
    Definition 78. A set A is infinite if there exists an injective 
                   function f :: A -> B such that B is a proper subset 
                   of A.

    Ans: A 'proper subset' is always at least one element smaller
         than a subset so |A| > |B| yet the properties on finite sets, 
         for interjection, require |A| <= |B|.
-}           
{-
    Exercise 22
    
    Suppose that your manager gave you the task of writing a program
    that determined whether an arbitrary set was finite or infinite.
    Would you accept it? Explain why or why not.
    
    Ans: No. A set is finite only if there is a bijection that will 
         map its elements to natural numbers, i.e. if it is countable. 
         However, infinite sets are also countable and the only way to 
         tell the difference is to reach the end of a count with a 
         finite set; which, if your arbitrary set happens to be infinite, 
         you will never reach so the attempt at distinguishing between the
         two is futile.
-}         
{-
    Exercise 23
    
    Suppose that your manager asked you to write a program that
    decided whether a function was a bijection. How would you respond?
    
    Ans:  A biejection function is both surjective and injective
          so if you prove a function is both of these it is also
          a biejection. So yes, it is possible to check if a
          function is bijective. (see isBijective function in Stdm)
-}
{-
    Exercise 24
    
    The software tools file contains a definition of a list named
    rationals, which uses the enumeration illustrated above. Try evaluating
    the following expressions with the computer:

            take 3 rationals
            take 15 rationals

-}
ex24a = take 3 rationals
ex24b = take 15 rationals

{-
    Output:
    
        Main> ex24a
        [(1,1),(1,2),(2,1)]
        Main> ex24b
        [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),
        (4,1),(1,5),(2,4),(3,3),(4,2),(5,1)]
        Main>     
-}
{-
    Exercise 25
    
    A program contains the expression (f.g) x.
    (a) Suppose that when this is evaluated, the g function goes into an
        infinite loop. Does this mean that the entire expression is 
        undefined?
        
        Yes.  (f.g) = f ( g x) 
              (g x ) will be executed first and never return, making
              'f' unreachable

    (b) Now, suppose that the application of f goes into an infinite loop.
        Does this mean that the entire expression is undefined?

        Yes. 'g' may be defined and return values correctly but we'll
        never see them as 'f' continually loops.

-}
{-
    Exercise 26
    
    Each part of this exercise is a statement that might be correct
    or incorrect. Write Haskell programs to help you experiment, so that 
    you can find the answer.

    (a) Let (f . g) be a function. If f and g are surjective then (f . g)
        is surjective.
    (b) Let (f . g) be a function. If f and g are injective then (f . g)
        is injective.
    (c) If (f . g) is bijective then f is surjective and g is injective.
    (d) If f and g are bijective then (f . g) is bijective. 

    Ans: (a) True,
         (b) True, 
         (c) False, either f, g or both functions can be surjective
             or injective
         (d) True
             
          

-}
-- experiment functions from provided solutions
surjectiveExperiment :: (Eq a, Show a) =>
                         Set a -> Set a -> Set (a, FunVals a) ->
                         Set a -> Set a -> Set (a, FunVals a) -> Bool
surjectiveExperiment dom_f co_f f dom_g co_g g =
        isSurjective dom_f co_f f /\ isSurjective dom_g co_g g
    ==> isSurjective dom_g co_f (functionalComposition f g)
    
    
surjExp :: Set Int -> Set Int ->
           Set Int -> Set Int -> Bool
surjExp domain_f image_f domain_g image_g =
    let f = [(x,Value y) | x <- domain_f, y <- image_f]
        g = [(x,Value y) | x <- domain_g, y <- image_g]
    in surjectiveExperiment [1..10] [1..10] f 
                            [1..10] [1..10] g

ex26a = surjExp [2,3,4] [4,6,8]     -- f  (*2)
                [1,2,3] [2,3,4]     -- g  (+1)
                
injectiveExperiment :: (Eq a, Show a) =>
                        Set a -> Set a -> Set (a, FunVals a) ->
                        Set a -> Set a -> Set (a, FunVals a) -> Bool
injectiveExperiment dom_f co_f f dom_g co_g g =
        isInjective dom_f co_f f /\ isInjective dom_g co_g g
    ==> isInjective dom_g co_f (functionalComposition f g)
    
injExp :: Set Int -> Set Int ->
          Set Int -> Set Int -> Bool
injExp domain_f image_f domain_g image_g =
    let f = [(x,Value y) | x <- domain_f, y <- image_f]
        g = [(x,Value y) | x <- domain_g, y <- image_g]
    in injectiveExperiment [1..10] [1..10] f
                           [1..10] [1..10] g
                           
ex26b = injExp [2,3,4] [4,6,8]     -- f  (*2)
               [1,2,3] [2,3,4]     -- g  (+1)
                                                      
                           
bijectiveExperiment :: (Eq a, Show a) =>
                        Set a -> Set a -> Set (a, FunVals a) ->
                        Set a -> Set a -> Set (a, FunVals a) -> Bool
bijectiveExperiment dom_f co_f f dom_g co_g g =
        isBijective dom_g co_f (functionalComposition f g)
    ==> isSurjective dom_f co_f f /\ isInjective dom_g co_g g
    
bijExp :: Set Int -> Set Int ->
          Set Int -> Set Int -> Bool
bijExp domain_f image_f domain_g image_g =
    let f = [(x,Value y) | x <- domain_f, y <- image_f]
        g = [(x,Value y) | x <- domain_g, y <- image_g]
    in bijectiveExperiment [1..10] [1..10] f
                           [1..10] [1..10] g  

ex26c = bijExp [2,3,4] [4,6,8]    -- f  (*2)
              [1,2,3] [2,3,4]     -- g  (+1)
                           
{-
    Exercise 27
    
    The argument and result types given here are sets, not expressions
    or types in Haskell. Given the functions
        f : {1,2,3} -> {4,5,6}
        f 1 = 4
        f 2 = 6
        f 3 = 5

        g : {4,5,6} -> {1,2,3}
        g 4 = 1    
        g 5 = 1
        g 6 = 2
        
    what is
        (g o f) 1       g(f 1) -> g(4) -> 1            
        (g o f) 3       g(f 3) -> g(5) -> 1
        (f o g) 4       f(g 4) -> f(1) -> 4
        (f o g) 5       f(g 5) -> f(1) -> 4
-}                           
f27 :: Int -> Int
f27 1 = 4
f27 2 = 6
f27 3 = 5

g27 :: Int -> Int
g27 4 = 1    
g27 5 = 1
g27 6 = 2

ex27a = (g27 . f27) 1       -- 1
ex27b = (g27 . f27) 3       -- 1
ex27c = (f27 . g27) 4       -- 4
ex27d = (f27 . g27) 5       -- 4

{-
    Exercise 29
    
    State the properties of the following functions:
        f : {3,4,5} -> {3,4,5}
        f 3 = 4
        f 4 = 5
        f 5 = 3

        g : {0,1,2} -> {0,1,2}
        g 0 = 0
        g 1 = 1
        g 2 = 2

        h : {3,4,5} -> {3,4,5}
        h 4 = 3
        h 5 = 4
        h 3 = 5    

    Ans:
        f is a permutation
        g is the identity function
        h is the inverse of f
-}
{-
    Exercise 29
    
    Given the functions
        f : {x,y,z} -> {7,8,9,10}
        f x = 8
        f y = 10
        f z = 7

        g : {7,8,9,10} -> {x,y,z}
        g 7 = x
        g 8 = x
        g 9 = x
        g 10 = x

        h : {7,8,9,10} -> {7,8,9,10}
        h 7 = 10
        h 8 = 7
        h 9 = 8
        h 10 = 9

    describe the following functions:    

        g o f
        h o f
        g o h    
        
    Provided solution:
        f is a constant function
        g is injective  (x can vary??)
        h is a constant function
-}        
{-
    Exercise 30
    
    Given the domain and codomain {1, 2, 3, 4, 5}, which of the following
    are functions?
        f 1 = 2
        f 2 = 3
        f 3 = 3
        f 3 = 4
        f 4 = 4
        f 5 = 5

        g 1 = 2
        g 2 = 1
        g 3 = 4
        g 4 = 4
        g 5 = 3

        h 1 = 2
        h 2 = 3
        h 3 = 4
        h 4 = 1    

    Ans:
        f - not a function, (f 3) gives two different outputs
        g - is a function
        h - is a not a function (doesn't map 5 to any value)
-}
{-
    Exercise 31
    
    Determine which of the following definitions are partial functions
    over the set {1, 2, 3}.
        f 1 = undefined
        f 2 = 1
        f 3 = 2

        g 1 = 3
        g 2 = 2
        g 3 = 1

        h 1 = undefined
        h 2 = undefined
        h 3 = undefined    

    Ans:
        f is a partial function
        g is a total function
        h is a partial function
-}
{-
    Exercise 32
    
    The following functions are defined over the sets {1, 2, 3} and
    {7, 8, 9, 10}.
        f 1 = 7
        f 2 = 8
        f 3 = 9

        g 7 = 1
        g 8 = 2
        g 9 = 3
        g 10 = 1

        h 1 = 3
        h 2 = 2
        h 3 = 1

    Which of the following are surjections?
        h o h
        f o g
        g o f
        h o f
        g o h    

    Ans:
    
        (h . h) 1 -> h(h 1) -> h 3 -> 1           surjective
        (h . h) 2 -> h(h 2) -> h 2 -> 2           A maps to all of B
        (h . h) 3 -> h(h 3) -> h 1 -> 3
        
        (f . g) 7 -> f(g 7)  -> f 1 -> 7          not surjective
        (f . g) 8 -> f(g 8)  -> f 2 -> 8          A does not map to
        (f . g) 9 -> f(g 9)  -> f 3 -> 9          every element in B
        (f . g)10 -> f(g 10) -> f 1 -> 7
        
        (g . f) 1 -> g(f 1) -> g 7 -> 1           surjective
        (g . f) 2 -> g(f 2) -> g 8 -> 2           A maps to all of B
        (g . f) 3 -> g(f 3) -> g 9 -> 3
        
        (h . f) - all results are undefined, not surjective
                  (domains don't match)
        
        (g . h) - all results are undefined, not surjective
                  (domains don't match)
        
        [If domains don't match, composition can't be surjective]
    
-}
{-
    Exercise 33
    
    The functions f, g, and h are defined over the sets {1, 2, 3} and
    {4, 5, 6}; which of them are injections?
        f 1 = 4
        f 2 = 5
        f 3 = 5

        g 4 = 1
        g 5 = 2
        g 6 = 3

        h 4 = 1
        h 5 = 1
        h 6 = 1    

    Ans:
        f - is not an injection, result set values are not unique
        g - is an injection
        h - is not an injection
-}                     
{-
    Exercise 34
    
    Consider the following functions defined over the sets {1, 2, 3}
    and {6, 7, 8, 9}; which of them are bijections?
        f 6 = 1
        f 7 = 2
        f 8 = 3
        f 9 = 3

        g 1 = 3
        g 2 = 2
        g 3 = 1

        h 1 = 6
        h 2 = 7
        h 3 = 8    

        (g . g)
        (h . f)
        (f . h)
        
    Ans:
        (g . g) 1 -> g(g 1) -> g 3 -> 1        yes, bijective
        (g . g) 2 -> g(g 2) -> g 2 -> 2
        (g . g) 3 -> g(g 3) -> g 1 -> 3
        
        (h . f) 6 -> h(f 6) -> h 1 -> 6        no, not 1-to-1
        (h . f) 7 -> h(f 7) -> h 2 -> 7
        (h . f) 8 -> h(f 8) -> h 3 -> 8
        (h . f) 9 -> h(f 9) -> h 3 -> 8
        
        (f . h) 1 -> f(h 1) -> f 6 -> 1        yes, bijective
        (f . h) 2 -> f(h 2) -> f 7 -> 2
        (f . h) 3 -> f(h 3) -> f 8 -> 3
-}
{-
    Exercise 35
    
    Which of these functions is a partial function?

    function1 True  = False
    function1 False = function1 False

    function2 True  = True
    function2 False = True    

    Ans:
        function1 is partial - never returns True
        function2 is partial - never returns False
        
    Provided solution:
    The first function is partial, because it does not terminate when
    given False, and so does not produce a result given that domain 
    value. The second function is total.
        
    [ Partial function if 'x' is not defined (does not produce a
      result) for all of it's possible values]
-}
{-
    Exercise 36
    
    Using normalForm and map, write a function that takes a list
    of pairs and determines whether the list represents a function. 
    You can assume in this and the following questions that the domain 
    is the set of first elements of the pairs and the image is the set 
    of second pair elements.    

-}         
-- provided solution
isFunction :: (Eq a, Show a) => Set(a,b) -> Bool           
isFunction xs = normalForm (map fst xs)

{-
    Exercise 37
    
    Using normalForm and map, define a function isInjection so
    that it returns True if the argument represents an injective function and
    False otherwise.    
    
-}
-- provided solution
isInjection :: (Eq a, Eq b, Show a, Show b) => Set (a,b) -> Bool
isInjection ps = normalForm (map fst ps) /\ normalForm (map snd ps)

{-
    Exercise 38
    
    Is it possible to write a function that determines whether a list
    of pairs represents a surjective function without passing in the 
    co-domain of the function?    
    
    Ans: A function is surjective if its image (range)
         includes every member of its result type; if we don't have
         the co-domain we cannot check that they are all found as
         the second element of each pair; although, if we know
         the result type and we have an inductively defined function
         then it would be possible for infinite sets (?? - no provided
         solution for this one)
         
-}
{-
    Exercise 39
    
    How much information would you need to know about a Haskell
    function in order to be able to tell that it is not the identity 
    function?
    
    Ans: An identity function input and output have same result type
         so you could definitely tell if a function is not an identity 
         function if the two types are different.

-}
{-
    Exercise 40
    
    Write a function with type

    compare :: (Eq a, Eq b, Eq c, Show a, Show b, Show c)
            => (a -> b) -> (b -> c) -> (a -> c) -> a -> Bool

    that takes three functions f, g, and h and determines whether 
    (f . g) = h for some value of type a.

-}
compare :: (Eq a, Eq b, Eq c, Show a, Show b, Show c)
        => (a -> b) -> (b -> c) -> (a -> c) -> a -> Bool
compare f g h a = h a == (g . f) a

{-
    Exercise 41
    
    Is this definition of isEven inductive?

        isEven :: Int -> Bool
        isEven 0 = True
        isEven 1 = False
        isEven n = isEven (n-2)

    Ans: No. It won't terminate if a negative number is passed in.
        [Yes for Natural numbers]
-}
{-
    Exercise 42
    
    Is this definition of isOdd inductive?
    
    isOdd 0 = False
    isOdd 1 = True
    isOdd n = if (n < 0) then isOdd (n+2) else isOdd (n-2)
    
    Ans: Yes. Will terminate regardless of the [Integer] values passed in
-}
{-
    Exercise 43
    
    A = {1, 2, 3, 4, 5}
    B = {6, 7, 8, 9, 10}
    D = {7, 8, 9, 10}
    C = {a, b, c, d, e}
    
    f :: A -> B, f = {(1, 7), (2, 6), (3, 9), (4, 7), (5, 10)}
    g = {(6, b), (7, a), (6, d), (8, c), (10, b)}
    
    1. Is f a function? Why or why not?
        Yes. Every input from A has an output.
    
    2. Is f injective (that is, one-to-one)? Why or why not?
        No. The inputs 1 and 4 both yield the value 7.
    
    3. Is f surjective (that is, onto)? Why or why not
        No. Not every element of B appears as a 'y' values.
    
    4. Is g a function? Why or why not?    
        No. The domain is {6,7,6,8,10} so the same input,6, has the
        potential to yield different outputs.
        
{-
    Exercise 44
    
    Let A = 1, 2, . . . , n. Suppose f :: A -> P(A), where P(A) is the
    power set of A.

    1. Prove that f is not surjective.
    
       There are 'n' elements in A but P(A) will have 2^n elements
       For a function to be surjective every element of input
       has a 1-to-1 correlation with the output, which can't be
       true when the output is 2^n.
    
    2. Suppose g :: X -> Y and the set (g.f )(A) is a subset of A 
      (same A and same f as before), where the dot operator (.) stands 
      for function composition. State the relationships among A, X, and Y .
      
      Provided solution:
       The composition g.f is defined when g is defined on the image 
       f (A). So, X is a subset of P(A). Furthermore, since (g.f )(A) is
       a subset of A, and since (g.f )(A) = g(f (A)), Y , the codomain of 
       g, must contain at least those elements of A that are in (g.f )(A).
       That’s one way to look at it. Here’s another way: The composition
       (g.f ) is defined when the codomain of f is the domain of g. Taking
       this position, X = P(A) and Y , the codomain of g contains at least
       g(f (A)), which, by hypothesis, is a subset of A. Either position 
       is ok. It depends on how the term “composition” is defined. Because
       we were not precise about when to consider the composition defined,
       either answer is ok.
      
      
    3. Can g be injective? Why or why not?
    
       Provided solution:
       
        Again, it depends on the details of the definition of composition.
        Taking the first view, the domain of g must contain, at least, 
        the set f (A). It may contain only this set, in which case it can
        be injective because the number of targets f can hit cannot
        exceed the number of bullets in A, so g will not have more 
        bullets than targets and can be defined to hit each potential
        target at most once. On the other hand, if P(A) is considered
        to be the domain of g, then g will have too many bullets for A. 
        So, if g is to be injective, its codomain must contain elements 
        other than those of A. The intersection of A and the codomain of 
        g will contain, at least, (g.f )(A).       
    
    4. Define f and g with the above domains and ranges such that (f .g)
       is bijective (again, the dot operator (.) stands for function 
       composition).    
       
       Provided solution:
       
       Define g : f (A) -> A as follows. For each s in f (A), let g(s) 
       be one of the elements in f −1(s). Any element will do. Just pick
       one, and let that be g(s). There must be at least one such element 
       because s is in f (A). Then (f .g) : f (A) -> f (A) will be 
       bijective.

-}        
        
        
        
        
        
        
        
        
        
        
        
        

-}