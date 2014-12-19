-- Informatics 1 - Functional Programming 
-- Lecture 8 - Lambdas, Sections, Function Composition, Variable Binding
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect08.pdf
--  http://groups.inf.ed.ac.uk/vision/VIDEO/2014/inf1fp.htm
--      video 13/10/2014 23:00 minute mark

-- Example function
f :: [Int] -> Int
f xs = foldr (+) 0 (map sqr (filter pos xs))
    where
        sqr x = x * x
        pos x = x > 0
        
{-
    Can it be simplified? Can we remove the helper functions?
    Well, this WON'T work
    
        f :: [Int] -> Int
        f xs = foldr (+) 0 (map (x * x) (filter (x > 0) xs))
        
    you CANNOT just substitute the helper functions in; the above
    produces a 'Not in scope 'x''
    
    What's missing is the function that produces the 'x'
    We can simplify to the following which uses 'lambda expressions'
-}        
f' :: [Int] -> Int
f' xs = foldr (+) 0
          (map (\x -> x * x)
            (filter (\x -> x > 0) xs))
            
{-
    Lambda Calculus created by logician Alonzo Church (1903-95)
    John McCarthy, inventor of the first functional language, LISP,
    in 1960, was a student of Church.
    
    The backslash '\' in the expression (\x -> x * x) represents
    the Greek lambda character.
    The left-arrow '->' is to remind us of the function type
    
    Example evaluation:
        (\x -> x > 0) 3
        ==> let x = 3 in x > 0
        ==> 3 > 0
        ==> True
        
        (\x -> x * x) 3
        ==> let x = 3 in x * x
        ==> 3 * 3
        ==> 9
        
        -- given two arguments
        (\x -> \y -> x + y) 3 4
        ==> ((\x -> (y -> x + y)) 3) 4     -- expression is curried
        ==> (let x = 3 in y -> x + y) 4
        ==> (\y -> 3 + y ) 4
        ==> let y = 4 in 3 + y
        ==> 3 + 4
        ==> 7
        
    In general terms, we evaluate a Lambda expression as:
        (\x, N) M = let x = M in N
        
    This is referred to as the 'Beta Rule'
    
    [Note: the lambda expressions DON'T have names; they are
           anonymous. Also notice the use of 'let'; this
           is how 'local' variables are created in Haskell.
           i.e. the 'scope' (visibility) of 'x' is LIMITED TO 
           THE DEFINING FUNCTION. (see Variable Binding at
           end of this file.)]
        
-}      
{-
    SECTIONS
        shorthand for lambda expressions
        
        (> 0)       (\x -> x > 0)
        (2 *)       (\x -> 2 * x)
        (+ 1)       (\x -> x + 1)
        (2 ^)       (\x -> 2^x)
        (^ 2)       (\x -> x^2)
        
        Essentially, sections are partial applications of binary operators
        you can supply, or 'fix', either the first or second argument
        the missing argument is assumed to be replaced by a generated
        variable 'x'.
        
        You can also create a section for ANY infix operator, so
        (`div` 3), (`mod` 5), etc. are also valid sections.
        
        Which means we can re-write our original function in one line,
        eliminating helpers
        
-}      
f'' :: [Int] -> Int
f'' xs = foldr (+) 0 (map (^ 2) (filter (> 0) xs))

{-
    FUNCTION COMPOSITION
        (.) dot operator
        used to combine two functions to create a third
        
            (.) :: (b -> c) -> (a -> b) -> (a -> c)
            (f . g) x = f (g x)
            
        So the dot operator works like any other binary operator,
        taking two arguments BUT the arguments must, themselves,
        be functions, as is the returned value; and, this is made 
        explicit in the the type signature: 
        
            first  argument  (b -> c)    is a function
            second argument  (a -> b)    is a function
            returned value   (a -> c)    is a function
            
        The type variables may look like they're in the wrong order
        but you can work it out by following an argument 'x':
                (b -> c) -> ((a -> b) -> (a -> c)) x
            ==> (b -> c) -> ((a -> b) -> (x -> c))
            ==> (b -> c) -> (x' -> b)              
            ==> (x'' -> c)
            
            f (g x) -> f x' -> x''
            
            where x   is an argument passed to the the final function
                  x'  is x  transformed by the fn (a -> c)     
                  x'' is x' transformed by the fn (a -> b)    

                   (g x) has type (a -> b)      argument 2
                 f (g x) has type (b -> c)      argument 1
                 (f . g) has type (a -> c)      result value
                  
        Little easier to visualize doing a walk through using two
        functions: sqr and pos
            
-}
sqr :: Int -> Int
sqr x = x * x

pos :: Int -> Bool
pos x = x > 0

ex1 = (pos . sqr) 3

{-
    (pos . sqr) 3       ((\x -> x > 0) . (\x -> x * x)) 3
    ==> pos (sqr 3)     ((\x -> x > 0) . (3 * 3))
    ==> pos  9          (\x -> x > 0 ) 9
                        (9 > 0)
    ==> True            True
    
    Composition (.) is Associative:
        f . g . h = f . (g . h)
        
           ((f . g) . h) x
        == (f . g) (h x)
        == f (g ( h x ))
        == f ((g . h) x )
        == (f . (g . h)) x

    and the identity function is:  f x = x
              
-}
{-
    We can now put everything together to further define our original 
    function definition to:
    
        f :: [Int] -> Int
        f xs = foldr (+) 0 (map (ˆ 2) (filter (> 0) xs))

        f :: [Int] -> Int
        f = foldr (+) 0 . map (^ 2) . filter (> 0)    
        
    An example walk through:
    
        f [1, -2 , 3]
        ==> (foldr (+) 0 . map (^ 2) . filter (> 0)) [1, -2, 3]
        ==> foldr (+) 0 (map (^ 2) (filter (> 0) [1, -2, 3]))
        ==> foldr (+) 0 (map (^ 2) [1, 3])
        ==> foldr (+) 0 [1, 9]
        ==> 10
             
    Similart to 'piping' or 'streaming' - a chain of functions
-}
{-
    VARIABLE BINDING
        Consider
        
            x = 2
            y = x+1
            z = x+y*y

            *Main> z
            11        
            
        In imperative progamming, this would be 'assignment' where the
        variables occupy a place in memory and 'x', 'y' and 'z' are
        references to the memory spaces occupied by the values.
        
        In Haskell, the equal sign 'binds', it DOES NOT ASSIGH. 
        i.e. x, y znd z are NAMES that are bound to the given VALUES
             x IS the VALUE 2
             y IS the VALUE x + 1 = 2 + 1 = 3
             z IS the VALUE x + y * y = 2 + 3 * 3 = 11
             
        The same VALUE can have different names.
        
        Once a variable (name) is 'bound' it is 'bound' to the value
        for life; here, 'x' will be '2' until the program ends.
        
        [Note: the 'binding' x = 2 says 'let x = 2 in this program';
               'local' to the enclosing program]
        
    Lambda expressions can explain binding in that a variable binding
    can be re-written as:
        N where x = M = (\x.N) M = (let x = M in N)
        
    A function binding can be re-written as:
        (M where f x = N) = (M where f = \x.N)
            
    Example:
    
        f 2
        where
            f x = x + y*y
            where
                y = x + 1
        
        = f 2 where f = \x -> (x+y*y where y = x+1)
        = f 2 where f = \x -> ((\y -> x+y*y) (x+1))
        = (\f -> f 2) (\x -> ((\y -> x+y*y) (x+1)))    
    
    EVERYTHING in FP can be turned into a Lambda expression
    and every Lambda expression can be written as two expressions, K and S
        K x y = x
        S x y z = (x z) (y z)
        
    [Note: select first, select second? Need to know more about Lambda
           calculus to understand this fully]
-}
