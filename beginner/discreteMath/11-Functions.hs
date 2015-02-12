-- Chapter 11 - Functions
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
import Stdm

{-
    Functions
    
    abstract - "A function is an abstract model of computation" 
             - essential quality is that the result is totally 
               determined by the input; same input always = same output
    
    black-box - don't need to know how it works in order to use if
              - only need to know what input it takes
                 
    Because the concept of a function is abstract there are many 
    different ways to define it.

-}
{--
    11.1 Graph of a Function
    
    A function can be represented as an ordered pairs (x,y)
    where 'x' is the input and 'y' the output; an entire 
    function can be represented by a 'set' of ordered pairs
    and a graph (much like relations and the digraph)
    
    Function Graph - a set of ordered pairs representing the
                     entire function
                   - a function is a relation with the additional
                     requirement that only one result may be
                     specified for each argument
                   - ie. for every input (x) there must be one, and
                         only one output, (y) i.e. every input value
                        'x' has one unique output value 'y'
                     
    Def: Let A and B be sets. A function, f, with type A -> B
         is a relation with domain A and co-domain B such that
         
         (all x in A).(all y1,y2 in B).
            ((x,y1)in f && ((xy2) in f) -> y1 == y2
            
        A is the argument type
        B is the result type
        
    Examples:
        {(1,4),(1,5)} - not a function, different y's for same x
        {(1,2),(2,2)} - a function, doesn't matter if different
                        input (x's) return same results (y)
                        
    Function Application
        The application of the function 'f' to the argument 'x'
        provided f::A->B and x::A is written as:  f x  or  f(x)
        and it's value is 'y' IF the ordered pair (x,y) is in 
        the graph of f, otherwise x is undefined
        
                f x = y <-> (x,y) in f
                
    Type
        A 'type' can be thought of as the set of all possible
        values that a variable might have.
        
        'x :: A' means 'x has type A' which means 'x in A'
        
        The type of a function is written A -> B which means
        the function takes an argument of type A and turns it
        into a result of type B.
        
        If x in A and (x,y) in f then 'f x is defined to be y'
        If x in A and no (x,y) in f the 'f x is undefined'
        
    Domain of a function (input, arguments)
        The subset of A consisting of arguments for which f is defined
        
                domain f = { x | (exists y).(x,y) in f }
                
        The set of all x such that (x,y) appears in the function graph
        
    Image of a function (output, results)
        The subset of B consisting of results actually returned by f
        
                image g = { y | (exists x).(x,y) in f }
                
        The set of all y such tat (x,y) appears in the function graph.
        
    Note: terminology differs; may see 'range' for 'image', 'co-domain'
          for 'domain'; check text usage
    
    Example:
        Let f :: Integer -> Integer be defined as
        
            f = {(0,1), (1,2), (2,4), (3,8)}
            
        The argument type of 'f' is Integer {...,-2,-1,0,1,2...}
        and its 'domain' is {0,1,2,3}
        
        The result type of 'f' is Integer {...,-2,-1,0,1,2,...}
        and its 'image' is {1,2,4,8}
        
-}
{-
    11.2 Functions in Programming
    
    In math, a function is a set of answers; in programming a function
    is a method for computing results.
    
    A function in programming is represented by an algorithm.
    The only way to determine 'f x' is by executing the algorithm on x
    
    A function has 'behaviour'; it consumes time and memory. 
    Functions that do essentially the same thing, i.e. sort a list
    can have different algorithms. The difference  can be important.
    
    The graph model of a function takes no notice of those differences
    as long as the algorithms produce the same results for the same
    inputs.
    
    There are several classes of functions; the essentials we're 
    interested in are their 'speed' and 'termination'.
    
    Inductively Defined Functions
    -----------------------------
        An 'inductively defined function'' is defined in the following 
        form, where 'h' is non-recursive:
        
                f 0 = k
                f n = h( f (n-1) )
                
            where, k is a constant
                   h does some calculation on the result of the
                     recursive call
            
        As long as 'h' always returns a result, the function will 
        always return a result when given a non-negative input.
        
        Examples:
            Summing number 0 through n
            
            f 0 = 0
            f n = n + f (n-1)
            
-}
-- add n to itself k times
add' :: Int -> Int -> Int
add' n 0 = 0
add' n k = n + add' n (k - 1)

-- this function is recursive BUT not inductively defined
-- it calls itself recursively on a 'larger' (not smaller)
-- argument
f91 :: Int -> Int
f91 x = if   x > 100
        then x - 10
        else f91 (f91 (x + 11))

{-
    Primitive Recursion
    -------------------
    
    Primitive recursion functions are equivalent to algorithm
    looping structures that are guaranteed to terminate.
    
    A function 'f' is a primitive recursion function if it has
    the following form and 'g' and 'h' are also primitive
    recursion functions:
    
            f 0     x = g x
            f (k+1) x = h (f k x) k x
            
    Examples:
    
    Basic Function
    --------------
    The sqr function takes a number and x and returns x^2
    
        sqr x = f 0 x
            where f 0 x = g x
                  g x   = x*x
                  
        The sqr function satisfies the primitive recursion function
        requirements 'vacuously' in that it does actually use 
        recursion.
        
    All 'basic functions' that do not require recursion can be handled
    in the same way; so they are all primitive recursive.
    
    Factorial
    ---------
        factorial k = f k undefined
            where f 0     x = 1
                  f (k+1) x = (k+1) * (f k x)
                  
         Performs a recursion over 'k' starting with the initial
         argument and counting down to 0 [Note: the (f k x) is
         the countdown]
         
         The 'x' argument is never actually used and so can be
         anything; the full power of primitive recursion is not
         being used.
         
    Not Primitive Recursive
    -----------------------
    
            f 0 = 0
            f 1 = 0
            f x =
                if even x
                then 1 + f (x `div` 2)
                else f x                    -- no change to arg
    
        The above is not primitive recursive because if it is
        called with any odd number other than 1 it will 
        continually call itself with the same argument.

    Computational Complexity
    ------------------------
    
    The computational complexity of a function is a measure of
    how costly it is to evaluate. Memory consumption and execution
    time are common measurements. 
    
    Recursion can create some very expensive computations; one such
    is Ackermann's which is cheap for small numbers but very 
    expensive as the input grows.
    
            ack 0 y = y+1
            ack x 0 = ack (x-1) 1
            ack x y = ack (x-1) (ack x (y-1))
     
    State
    -----
    
    Functions always return the same value given the same input; this
    is not true for all computations i.e. System.DateTime always returns
    a different value. 
    
    The entire set of circumstances that effect the result of a 
    computation is called the 'state'.
    
        Examples:
        
        The state of a computer system includes the current date,
        time of day and hd contents. 'Functions' that query these
        elements constantly return different values and so they
        are not 'true' functions (even though they are called that).
        
        Any function that modifies system 'globals' is not a function
        even if it, itself, always returns the same result for the
        same input [functions with side-effects are not mathematical
        functions]
        
    It is possible to describe computations with state as true
    mathematical functions by adding the 'state' as an extra
    argument:
    
                    f :: State -> Integer -> Integer
                    
    'f' can return a value that depends on the time of day even
    though it is a mathematical function. [technically, returning
    the same result for the same input if we consider State in
    the abstract]
    
    Imperative programming makes the 'state' implicit, rather than
    passing it as an argument; it allows 'side effects' at the cost
    of giving up 'equational reasoning'; if 'f x' depends on the 
    system state than 'f x' is not equal to 'f x'. There are still
    ways to reason about the program but they are not as effective
    as equational reasoning.
    
    Haskell provides the 'monad' as a mechanism for dealing with
    state implicitly [Text recommends "Haskell: The Craft of Functional
    Programming" by Simon Thompson for info on using monads]
    
-}
{-
    11.3 Higher Order Functions
    
    Functions that take and/or return other functions are called
    'higher order functions'.
    
    Example:
        The 'map' function takes another function as an argument
        
            map :: (a -> b) -> [a] -> [b]
            
            
    Functions that take functions as arguments
    ------------------------------------------
        
    All such functions are 'higher order' and have a type similar
    to:
            f :: (...->...) -> ...
            
    Generally these functions also take a data argument and
    apply the passed function to the passed argument
    
    Examples:
    
        map :: (a->b) -> [a] -> [b]
        map f [] = []
        map f (x:xs) = f x : map f xs
        
        'map' takes a function and a list, applies the function
        to elements of the original list, adding them to a new
        list which it returns as the result
    
        
        twice :: (a -> a) -> a -> a
        twice f x = f (f x)
        
        'twice' takes a function and a piece of data; it then
        applies the function to the data two times.
        
    The 'type signature' in 'twice' is more restrictive than the
    type signature for map; map can take a function of one type (a)
    and return a value of the  same or another type (b) while twice
    can only return values of the same type as the input.
    
    ie map can take a function that might take an integer and return
       a Bool while twice can only take a function that takes and
       returns an integer.
       
    Higher order functions provide a flexible and powerful approach
    to user defined control structures (for loops, whiles and if
    statements)
    
    Examples:
                
        ys = map f ys       equiv to        for i = i to n
                                               y[i] := f( x[i] )
                                               
        y = foldl f a xs    equiv to        y := a
                                            for i := 1 to n do
                                                y :=  f y x[i]
                
       
    Functions that return functions
    -------------------------------
    
    Any function that returns another function will have the
    following form:
    
                f :: ... -> (... -> ...)
                
    To understand this type of function it is useful to study
    its function graph
-}
-- some first order functions, they all take an int from [1..3]
-- and return a function unique constant value
ident, double, triple, quadruple :: Int -> Int

ident 1 = 1
ident 2 = 2
ident 3 = 3

double 1 = 2
double 2 = 4
double 3 = 6

triple 1 = 3
triple 2 = 6
triple 3 = 9

quadruple 1 = 4
quadruple 2 = 8
quadruple 3 = 12

{-
    The graphs of the above first order functions are straight-forward
    pairs:
    
        ident     = {(1, 1), (2, 2), (3, 3)}
        double    = {(1, 2), (2, 4), (3, 6)}
        triple    = {(1, 3), (2, 6), (3, 9)}
        quadruple = {(1, 4), (2, 8), (3, 12)}            

-}
-- a higher order function that takes an Int and returns one of
-- the above first order functions
multby :: Int -> (Int->Int)
multby 1 = ident
multby 2 = double
multby 3 = triple
multby 4 = quadruple

exMultBy = multby 3 2       -- 6

{-
    walk through:
          multby 3 2            
        = (multby 3) 2          -- Haskell syntax rule
        = triple 2              -- definition of multby (3rd equation)
        = 6                     -- definition of triple (2nd equation)
        
    The graph of a function that returns another function is a set
    of ordered pairs where 'x' is the input arg and 'y' is another
    function graph. For example, the graph of multby is:
    
        multby = {(1, {(1, 1), (2, 2), (3, 3)})
                  (2, {(1, 2), (2, 4), (3, 6)})
                  (3, {(1, 3), (2, 6), (3, 9)})
                  (4, {(1, 4), (2, 8), (3, 12)})}
                  
    The result, the 'y' element, of the ordered pair depends on
    the value of the 2nd input element and the resulf of the
    related function call.  Note that 'multby 1 2' will always
    return 2 and 'multby 3 3' will always return 9 so the same
    input will always return the same output; the output simply
    varies on the 2nd argument.
    
    Multiple Arguments as Tuples
    ----------------------------
    
    Technically, any function can take only one argument and return
    one result. To get around this we can package arguments, or
    results, as tuples.
    
    For example, a function that take two arguments can package them
    in one tuple
    
            add :: (Integer, Integer) -> Integer
            add (x,y) = x + y
            
    The graph of the above function is an infinite set, as Integer
    is an infinite set of ordered pairs with the 'x' element being
    a tuple:
    
        add = {
            ··· ,
            ... ,((0,−2),−2),((0,−1),−1),((0, 0), 0),((0, 1), 1), ...
            ... ,((1,−2),−1),((1,−1), 0),((1, 0), 1),((1, 1), 2), ...
            ... ,((2,−2), 0),((2,−1),−1),(2, 0), 2),((2, 1), 3), ...
            ···   
             }            
    
    Multiple Results as a Tuple
    ---------------------------
    
    Results can also be packaged in a tuple which is returned as
    a single value:
    
    Example:
            addsub1 :: Integer -> (Integer,Integer)
            addsub1 x = (x-1, x+1)
            
    The graph of the above is:
    
        addsub1 = {
            ··· ,
            (−2, (−3,−1)),(−1, (−2, 0)),(0, (−1, 1)),(1, (0, 2)),
            (2, (1, 3)),(3, (2, 4)),(4, (3, 5)),(5, (4, 6)),
            ...
             }
             
    Here, the 'y' element of the ordered pair is a tuple.
    
    Multiple Arguments with Higher Order Functions
    ----------------------------------------------
    
    Higher order functions offer another way to package multiple
    arguments.
    
    Suppose a function needs to take two arguments of type x::a
    and y::b and return a result of type 'c'. We can define the
    functon as:
    
                f :: a -> (b -> c)
                
    So, 'f' takes one argument of type 'a' and returns a function
    of type b -> c; the result function is ready to take an
    argument of type b and return a type c. This mechanism for
    packaging arguments is called 'currying'.
    
    The graph of the function is a set to ordered pairs where the
    first element is the type a data and the second element is
    the function graph of the result function.
    
    For example,
    
        mult :: Integer -> (Integer->Integer)
        mult x y = x*y
        
        The graph of mult is the set of ordered pairs (k, f_k)
        where 'k' is the value of the first argument to 'mult'
        and f_k is the graph of the function that takes a number
        and multiplies it by 'k'.
        
            mult = {
                · · · ,
                (−1,{. . . , (−1, 1), (0, 0), (1,−1), (2,−2), . . .}),
                (0, {. . . , (−1, 0), (0, 0), (1, 0), (2, 0), . . .}),
                (1, {. . . , (−1,−1), (0, 0), (1, 1), (2, 2), . . .}),
                (2, {. . . , (−1,−2), (0, 0), (1, 2), (2, 4), . . .}),
                (3, {. . . , (−1,−3), (0, 0), (1, 3), (2, 6), . . .}),
                ...
                 }
-}
{-
    11.4 Total and Partial Functions
    
    The 'domain' of a function f :: A -> B is the subset of A 
    containing all elements for which f is defined.
    
    Alternatively, we can say the domain of f is the set of
    all arguments of type A that produce a result.
    
    If the domain of f = A then 'f' is a 'total function'
    
    If the domain of f is a subset of A then 'f' is a 'partial function'
    
    If 'f' is a partial function and 'x' is in the domain of 'f', then
    'f x' is defined. 
    
    If 'f y' where 'y in A' but 'y not in domain f' then 'y' is 
    undefined. Computer science uses a special symbol (an inverted T)
    to represent an undefined value; the symbol is called 'bottom'.
    
    Example
    
        f :: Integer -> Char
        f 1 = ’a’
        f 2 = ’b’
        f 3 = ’c’
    
        'f' as an argument type of Integer and a domain of {1,2,3}
        'f 1' is defined as 'a' but 'f 4' is undefined (has no value).
        
        The graph of f is {(1, 'a'), (2, 'b'), (3, 'c')}.
    
    A function type signature acts as a constraint; if a non-Integer is
    passed to 'f' the compiler will throw an error. If we pass an Integer
    that is not in the domain of 'f' to 'f' then we get a runtime error.
-}
f :: Integer -> Char
f 1 = 'a'
f 2 = 'b'
f 3 = 'c'

{-
    Haskell allows us to define our own error messages
-}
fe :: Integer -> Integer
fe x =
    if x >= 0
    then 10*x
    else error ("fe was applied to a negative number "
                ++ show x ++ ". Don't do it again!")
                
{-
    Output:
    
    Main> fe (2+2)
    40
    Main> fe (2-5)

    Program error: f was applied to a negative number -3. 
       Don't do it again!

    Main>     

-}          
{-
    Sometimes we create infinite loops that; in which case no
    error will be detected and we have to use CTRL-C to break
    out of the computation.
    
    An application of a total function will always terminate
    and produce a result.
    
    The application of a partial function can have one 3 results:
        1. terminates and returns a result
        2. produces a runtime error
        3. never terminates
        
    Unfortunately there is no way to prove that a partial function
    will terminate; this is known as the 'Halting Problem'.
    
    While we can't write software tools to determine if a function
    is total or partial, we can create data structures to represent
    graphs of partial functions. The function requires every
    element of the domain to be mapped to some element of the
    result type, which can be the undefined value. So we need a 
    new type to represent a 'function value':
    
            data FunVals a = Undefined | Value a
                 deriving (Eq, Show)
                 
-}      
{-
    11.5 Function Composition
    
    It is always possible to create a sequence of functions; like
    a pipeline [stream] where the output of one function is the
    input of another.
    
        Let g :: a -> b and f :: b -> c. Then the composition
        of f with g is:
        
                (f . g) :: a -> c
                (f . g) x = f (g x)
                
    The type signature of the composition operator (.) is:
        (.) :: (b -> c) -> (a -> b) -> c
        (f.g) x = f (g x)
        
    Example:
        If you want to increment the second element in an 
        list of pairs you could write
        
            map increment (map snd lstOfPairs)
            
        or
        
            map (increment . snd) listOfPairs
            
    Functional composition is associative, that is, for all
    functions h::a->b, g::b->c, f::c->d
            f . (g . h) = (f . g) . h
    
    and both sides of the equation have type a->d
    
    Function application takes precedence, in Haskell, over all
    other operations so parentheses are not required in statements
    such as:
    
        g = f1 . f2 . f3 . f4       -- function definition
        
    If we want to apply the pipeline to an argument and save the
    resulting value, the entire pipeline goes in parentheses.
    
        y = (f1 . f2 . f3 . f4) x
    
    Stdm.lhs contains a function, 'functionComposition' which
    takes two function graphs and returns it's composition as
    another function graph.
-}
{-
    11.6 Properties of Functions
    
    We've used 4 sets to characterize functions:
        the argument type
        the domain
        the result type
        the image
        
    There are several useful properties of functions that concern
    these 4 sets.
    
    Surjective Property
    -------------------
    A surjective function has an image (range) the same as its 
    result type.
    
    Def:  f :: A -> B is surjective if
            (forall b in B).((exists a in A).f a = b)
            
    i.e. for a function to be 'surjective', the function image (result set) 
         must include every member of the result type         
            
    Example
        The image (result set) of a function with a Bool result type
        is surjective if it includes both True and False results.
        
        The increment function is surjective as the image set includes
        all integers.
        
        The even function is NOT surjective as its result type is
        Int but its image (result set) includes only even integers.
        
    If the result type of a function is larger than its domain it cannot
    be surjective. For example, let A = {2,3} and B = {4,5,6}, if
    f :: A -> B then it is not surjective as the image can only
    contain two ordered pairs since the 'x' value in every ordered
    pair must be unique; there cannot be a 'y' value for every element
    of B.
    
    Haskell functions that are surjective (result type are no larger
    than their domain types)
        not :: Bool -> Bool
        member v :: [Int] -> Bool
        increment :: Int -> Int
        id :: a -> a
    
    while the following are NOT surjective
        length :: [a] -> Int     -- returns only 0 or a pos integer
        abs :: Int -> Int        -- only returns pos integers, no negs
            
    Injective Property
    ------------------
    A function is 'injective' if each element of the image is the
    result of applying the function to exactly one element of the domain
    i.e. each 'y' element in a set of ordered pairs must be unique
    
    Def:  f :: A -> B is injective if
            (forall a, a' in A). a /= a' -> f a /= f a'
            
    [Note: result must include an 'x' value for EVERY 'a' value]
            
    Examples
        Let A = {1,2}, B = {3,4,5} and define f :: A -> B as
        {(1,3),(2,5)} then 'f' is injective as the 'x' element
        in each ordered pair has a unique 'y' element.
        
        Let A = {1,2,3}, B = {4,5} and define g :: A -> B as
        {(1,4),(2,5),(3,5)} then 'g' is NOT injective as '5'
        appears as the 'y' value in two pairs.
        
        Haskell functions that are injective:
            (/\) True :: Bool -> Bool
            increment :: Int -> Int
            id :: a -> a
            
        Haskell function that are NOT injective:
            length :: [a] -> Int
            take n :: [a] -> [a]      

            as in both cases, different inputs can be mapped
            to the same results i.e. multiple lists can have the
            same length; 'take' will map both [a1, a2, . . . , an, x] 
            and [a1, a2, . . . , an, y] to the same result
            [a1, a2, . . . , an], even if x /= y.
            
    Example 126

        Let A = {1, 2, 3} and B = {4, 5, 6}. Define three functions as
        follows:
            f1, f2, f3 :: A → B
            f1 = {(1, 4), (2, 6), (3, 5)}
            f2 = {(1, 4), (2, 4), (3, 5)}
            f3 = {(1, 4), (3, 5)}    
            
        Represent them as graphs and use the software tools to explore
        their properties.
        
        [Note: 'undefined' is part of every image??]
-}
fun_domain   = [1,2,3]
fun_codomain = [4,5,6]

fun1 = [(1, Value 4), (2, Value 6), (3, Value 5)]
fun2 = [(1, Value 4), (2, Value 4), (3, Value 5)]
fun3 = [(1, Value 4), (2, Undefined), (3, Value 5)]

ex126a = isInjective fun_domain fun_codomain fun1       -- True
ex126b = isInjective fun_domain fun_codomain fun2       -- False
ex126c = isInjective fun_domain fun_codomain fun3       -- True

ex126d =isInjective fun_domain fun_codomain 
                   (functionalComposition fun1 fun2)    -- False
ex126e = isInjective fun_domain fun_codomain 
                    (functionalComposition fun1 fun3)   -- False
ex126f = isInjective fun_domain fun_codomain 
                    (functionalComposition fun2 fun3)   -- False

{-
    The PigeonHole Principle
    ------------------------
    If A and B are finite sets and |A| > |B| there is no
    injection from A to B as every 'f a' must have a unique
    'b' result; that is not possible if there is not at least
    one 'b' for every 'a'. i.e. if A is the pigeons and B
    is the pigeonholes, if there are more pigeons than holes
    some pigeons will fly free.
    
    Theorem: Let A and B be finite sets such that |A| > |B|
             and |A| > 1. Let f :: A -> B, then
             
             (exists a, a' in A).(a /= a') /\ (f a = f a')
             
-}                    
{-
    11.7 Bijective Functions
    
        A function is 'bijective' if it is both surjective and
        injective i.e. A and B have a one to one correspondence.
                    |domain f| = |image f|
                    
        For every 'a' in the set A, there is a unique 'b' from
        the set B.
        
        Permutations
        ------------
            A 'permutation' is a bijective function f::A->A
            ie. it must have the same domain and image
            
            A permutation can only shuffle its input, nothing
            new can appear in its results.
            
        Example
            If A = {1,2,3} and f:: A -> A is defined as
            {(1,2),(2,3),(3,1)} then 'f' is a permutation.
            
            Functions that reorder lists are permutation functions
            i.e. sort, reverse
            
            If 'f' and 'g' are permutations functions then the
            composition (f . g) is also a permutation function.
            
        Function Inverse
        ----------------
        If f:: A -> B then the inverse of 'f' is f^(-1):: B -> A
        
               {(y, x) | (exists x, y). (x, y) in f}
               
            ie for every (y,x) in f^(-1) there exists an (x,y) in f
            
        Example
            Let A = {1,2,3} and B = {4,5,6} and f::A->B is defined
            as {(1,4),(2,5),(3,6)} then the graph of its inverse
            is  {(4,1),(5,2),(6,3)} and the type of its inverse is
            f':: B -> A
            
            The Haskell decrement function is the inverse of the
            Haskell increment function.
-}
{-
    11.8 Cardinality of Sets
    
    The size of a set is called its 'cardinality'.
    
    Def: A set S is finite if and only if there is a natural number n such
         that there is a bijection mapping the natural numbers 
         {0, 1,... , n - 1} to S.  The cardinality of S is n, and it is 
         written as |S|.
         
        i.e. if S is finite, it can be counted, and the cardinality
             is the total count of the number of elements in the set.
             
    Def:
        A set 'A' is infinite if there exists and injective function
        f::A->B such that B is a proper subset of A.
        
    Using the properties of a function over a finite set, we can say:
        
            if f is a surjective function |A| >= |B|
            if f is an injective function |A| <= |B|
            if f is a bijective function  |A| == |B|
            
    Def: Two sets have the same size (cardinality) if there is a
         bijection f::A -> B
         
    We can check the cardinality of infinite sets by relating them
    through a bijection function
    
    Example
        We can place set of integers, I, into a one to one 
        correspondence with the set of Natural numbers, N,
        
            N = 0  1 2  3 4  5 6 ...
            I = 0 -1 1 -2 2 -3 3 ...
            
        using the function f:: I -> N defined as
        
            f x | x >= 0    =  2 * x
                | otherwise = -2 * x - 1
                
        so the size of the set of Integers is the same as the size
        of the set of Natural Numbers and as the set of even
        numbers!
        
        [Wow! that's kind of mind boggling; tendency to think of
         Integers as every Natural number paired with its own
         negative which makes you think there'd be twice as many
         Integers as Natural numbers. 
         
         What does this say, then, about negative numbers? Can
         they be 'real' in the sense that there is any physical
         representation of a negative number? Or is 'neg' simply
         a useful property/classification that can be applied
         to a Natural number? A property created solely to 
         allow us to count numbers? If a have a '1' and then I
         have a 'not 1' what is the physical representation  
         of 'not 1'?  1 plus 'not-one' is zero ... is 'zero' really
         infinity? zero is not any other number, natural, integer,
         real, irrational, etc. It is the infinity of non-numbers
         of every number combined with it's 'not-self'; all number
         lines disappear into the rabbit hole of zero. 
         
         And what about even numbers?? Again there are as many
         as there are natural numbers; not 'half as many' so
         evenness is, again, just a property of natural numbers
         and not a 'real' thing in and of itself? Does this
         hold true for any other property; any other way we 
         can think of to classify numbers ... can we always equate them
         to natural numbers using a bijection??? And can we equate
         real, rational, irrational numbers in the same way?]
    
    Def: A set 'S' is countable if an only if there is a bijection
         f:: N -> S.
         
    A set is 'countable' iff it has the same cardinality as the
    set of natural numbers. i.e. a set is countable if it can
    be enumerated and this holds for infinite as well as finite sets.

    Rational Numbers are Countable
    ------------------------------
    A rational number is a fraction in the form x/y where x and y
    are integers; 'x' is the numerator, 'y' the denominator. 
    
    If we want to count all ratios we need to put them into a
    one-to-one correspondence with N. We can do this by creating
    a series of columns, each of which has an index, n, indicating
    its place in the series.
    
        (1, 1)
        (1, 2) (2, 1)
        (1, 3) (2, 2) (3, 1)
        (1, 4) (2, 3) (3, 2) (4, 1)
        (1, 5) (2, 4) (3, 3) (4, 2) (5, 1)
           .      .      .      .      .
           .      .      .      .      .
           .      .      .      .      .
           
    A column gives all possible fractions with 'n' as the numerator
    Every line in the sequence is finite and can be printed; every
    ratio will eventually appear, therefore, the set Q of rational
    numbers can be placed in a one-to-one correspondence with
    the Natural numbers and is countable.
    
    Real Numbers are Uncountable
    ----------------------------
    If A is a subset of B then we cannot say B is larger than A
    as they might be equal. Even if we know B is a proper subset
    of A we can still not say that A is greater than B as they
    might both be infinite.
    
    Consider a real number to be a string of digits. Consider the
    real numbers 'x' such that 0 <= x <= 1 which can be written,
    without limit, in the form: .d0d1d2d3...
    
    Now suppose there is some way we can put the real digits into
    a correspondence with natural numbers and we can make a table
    where the ith row contains the ith real number x_i and it
    contains the list of digits comprising x_i and these digits
    can be named d_i,0 d_i,1 d_i,2, ..., d_i,j where d_i,j means
    the jth digit in the decimal representation of ith real number
    x_i. We'd get a table somewhat like the following:
    
            .d00 d01 d02 d03 . . .
            .d10 d11 d12 d13 . . .
            .d20 d21 d22 d23 . . .
            ...  ... ... ... . . . 

    We can show this table is incomplete by constructing a new
    real number, y, which is definitely not in the list. This
    number will have the decimal representation 
    .dy0dy1dy2.... We can ensure that y is different from x0
    by defining the function:
    
            Different:: Digit -> Digit
            different x | x /= 0    = 0
                        | otherwise = 1
                        
    Note that it doesn't matter how different is defined as 
    long as it returns a value different than its argument.
    
    To ensure that y is different than x for every i in N
    define 
            dyi = different xii
            
    We have now defined a new number that is 'real' since
    it is defined by a sequence of numbers and it is different
    from x_i for any i; furthermore, our construction will
    work for any enumeration x_i, therefore, it is impossible
    to set up a correspondence between a set of R of reals
    and a set N of natural numbers and Real numbers are therefore
    uncountable.
      
-}
