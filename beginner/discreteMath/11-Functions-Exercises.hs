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

