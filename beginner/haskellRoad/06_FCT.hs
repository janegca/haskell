{-
    Chapter 6 - Functions
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
import Data.List
import Data.Char (isAlpha)

{-
    6.1 Basic Notions
    -----------------
        function  - transforms one object into another
        arguments - the objects given to a function
        values    - the results of the transformation (image)
        domain    - set of function arguments
                    a function is 'defined on its domain'
        
        If a function f has one argument 'x', it's corresponding
        value is: f(x).  A function value: y = f(x) is the 'image'
        of x under f.
        
        The domain of f is 'dom(f)'
        The 'range' of f is 'ran(f) = {f(x) | x in dom(f)}
        
    Example:  f(x) = x^2 + 1
        
    Def 6.2:
        A 'function' is a relation f that satisfies the following
        condition:
        
                    (x,y) in f /\ (x,z) in f => y = z
                    
        ie for every x in dom(f) there is 'exactly one' y in ran(f)
           such that (x,y) in f.
           
    If x in dom(f), then f(x) is, by definition the 'unique object'
    y in ran(f) for (x,y) in f. Where the domain: dom(f) of
    the function f is { x | (exists y) ((x,y) in f))}
    
    So, if 'f' is a function then:
    
        f      = {(a,f(a)) | a in dom(f)}
        ran(f) = {f(a) | a in dom(f)}
        
    An empty relation is a function: dom{} = {} = ran{}
        
    In computing, a function is an 'algorithm' for computing values.
    Following functions allow us to switch between set-theoretic 
    perspective and computational perspective.
-}
list2fct :: Eq a => [(a,b)] -> a -> b
list2fct [] _ = error "function not total"
list2fct ((u,v):uvs) x | x == u    = v
                       | otherwise = list2fct uvs x

fct2list :: (a -> b) -> [a] -> [(a,b)]
fct2list f xs = [ (x, f x) | x <- xs ]

-- the range of a function implemented as a list of pairs
ranPairs :: Eq b => [(a,b)] -> [b]
ranPairs f = nub [ y | (_,y) <- f ]

-- if a function is defined on an enumerable domain, we can list
-- its values
listValues  :: Enum a => (a -> b) -> a -> [b]
listValues f i = (f i) : listValues f (succ i) 

-- if the domain is bounded, we can generate the whole range as a
-- finite list
listRange :: (Bounded a, Enum a) => (a -> b) -> [b]
listRange f = [ f i | i <- [minBound..maxBound] ]

{-
    Examples:
    
    {(x,y) | x in R /\ y = x^2 +1} = {(x,x^2+1)|x in R}
    f = {(1,4),(2,4),(3,6)} is a function with dom(f) = {1,2,3}
         and ran(f) = {4,6}
    
    {(1,4),(2,4),(2,6)} is NOT a function (same input gives different
                        outputs)
                        
    
    From..to, On, Co-domain
    -----------------------
        Suppose that X and Y are sets. 
        A function 'f' is 'from X to Y'
        
                    f : X -> Y
                    
        IF dom(f) = X and ran(f) is a subset of Y (Y is called the
        'co-domain of f')
        
        A function is 'defined on X' if dom(f) = X.
        
        Note that it is not possible to recover the co-domain
        from R = { (x,f(x)) | x in X}, the co-domain could be
        any set ran(R).
        
    More than one argument
    ----------------------
        The functions described so far are 'unary'; however,
        functions that take more than one argument can be
        viewed as unary on ordered pairs, triples, etc.
        
        Haskell provides the 'curry' and 'uncurry' functions
        to allow us to easily switch between functions of
        type (a -> b) -> c to functions of type a -> b -> c
        and vice versa. The concept can be extended to triples, 
        etc.
-}
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z) 

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d 
uncurry3 f (x,y,z) = f x y z 

{-
    Function Equality
    -----------------
    If f and g are functions having the same domain and, on it,
    carry out the same transformation, they are equal.
    
    To establish that two functions are not equal we have to
    find an x in X with f(x) /= g(x); there is no generic
    algorithm that allows us to check for function equality;
    we need a proof.
    
    Functions are distinguished solely by their output behaviour,
    not on the details of how they perform their transformations.
    
    If f:X -> Y and g:X -> Z and (forall x in X)(f(x)=g(x)), then
    f and g are equal only if Y = Z.
-}
-- following equations all define the same function
f1 x = x^2 + 2 * x + 1
g1 x = (x + 1)^2
f1'  = \x -> x^2 + 2 * x + 1
g1'  = \x -> (x + 1)^2

ex68 x = f1 x == g1 x && f1' x == g1' x && f1 x == f1' x

{-
    Recurrences and Closed Forms
    ----------------------------
    A definition for a function f: N -> A, in terms of algebraic
    operations, is a closed form definition.
    
    A function definition for 'f' in terms of the values of 'f' for
    smaller arguments is called a 'recurrence' for 'f'.
    
    Closed forms allow for a more efficient computation than 
    recurrence forms as the computation time does not grow
    exponentially  with the size of the argument.
    
-}
-- function sums all numbers between 1 and n
-- recurrence (recursive)
g69 0 = 0
g69 n = g69 (n-1) + n

-- closed form definition of the same function
g69' n = ((n+1) * n) / 2

-- Exercise 6.10 - give a closed form definition of the following
ex610 0 = 0
ex610 n = ex610 (n-1) + (2*n)

ex610' n = (n+1) * n

-- Exercise 6.11 - give a closed form definition of the following
ex611 0 = 0
ex611 n =  ex611 (n-1) + (2*n-1)

ex611' n = n * n

{-
    Not always easy to find a closed form; take, as example,
    the factorial function; computationally, there is no
    difference between the following ('product' just hides
    the recursion)

-}
fac 0 = 1
fac n = fac (n-1) * n

fac' n = product [1..n]

{-
    Def 6.12 Restrictions
    ---------------------
        Suppose f:X -> Y and A is a subset of X.
        The 'restriction of f to A' is the function h:A->Y
        defined by h(a) = f(a).
    
        Following is an example for functions implemented as
        type a -> b and another for functions on lists of pairs
        
    [Note: the 'restriction' is determined by the class constraint
           the arguments must belong to the 'Eq' class]
-}
restrict :: Eq a => (a -> b) -> [a] -> a -> b
restrict f xs x | elem x xs = f x
                | otherwise = error "argument not in domain"
                
restrictPairs :: Eq a => [(a,b)] -> [a] -> [(a,b)]
restrictPairs xys xs = [ (x,y) | (x,y) <- xys, elem x xs ]

{-
    Def 6.13 Image and Co-image
    ---------------------------
    Suppose that f :X->Y, A is a subset of X and B is a subset of Y
    
        f[A] = {f(x) | x in A} is called the 'image of A under f'
        f^(-1)[B] = {x in X| f(x) in B} is called the 'co-image of B
                                        under f'
                                        
    from which we get
        f[X]      = ran(f)          -- range
        f^(-1)[Y] = dom(f)          -- domain
        y in f[A] <-> (exists x in A(y = f(x))
        x in f^(-1)[B] <-> f(x) in B
        
-}         
-- image of f is all unique results of (f x) for every x in xs   
image :: Eq b => (a -> b) -> [a] -> [b]
image f xs = nub [f x | x <- xs]

-- the coimage of f is all x in xs where the value (f x) is in ys
coImage :: Eq b => (a -> b) -> [a] -> [b] -> [a]
coImage f xs ys = [x | x <- xs, elem (f x) ys ]

ex613a = image (*2) [1,2,3]                 -- [2,4,6]
ex613b = coImage (*2) [1,2,3] [2,3,4]       -- [1,2]

-- image and coImage for lists of pairs
imagePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b]
imagePairs f xs = nub [y | (x,y) <- f, elem x xs]

coImagePairs :: (Eq a, Eq b) => [(a,b)] -> [b] -> [a]
coImagePairs f ys = [x | (x,y) <- f, elem y ys]

{-
    Exercise 6.14
    
    Consider the relation R = {(0,4), (1,2), (1,3)}
    
    1. I R a function?
            No. The input 1 gives two different output values.
            
    2. Remember: R^(-1_ = {(b,a) |(a,b) in R} is the inverse of the
       relation R.
            Is R^(-1) a function?
                R^(-1) = {(4,0), (2,1), (3,1)}  so Yes.
                
            If so, determine dom(R^(-1)) and ran(R^-1)
                domain is {2,3,4}
                range  is {0,1}                           
-}
{-
    Exercise 6.16
    
    Let X = {0,1,2,3}, Y = {2,3,4,5}
        f = { (0,3), (1,2), (2,4), (3,2) }
    Determine
        f restricted to {0,3}       -> {(0,3), (3,2)}
        f[{1,2,3}]                  -> { (1,2), (2,4), (3,2) } NOPE!
        inv f [{2,4,5}              -> { (2,1), (2,3), (4,2) } NOPE!
    Check your answers with the implementation
    
-}
-- provided solution
ex616a = [(0,3), (1,2), (2,4), (3,2) ]
ex616b = list2fct ex616a

ex616c = fct2list (restrict ex616b [0,3]) [0,3]  -- [(0,3),(3,2)]
ex616d = image ex616b [1,2,3]                    -- [2,4]
ex616e = coImage ex616b [0,1,2,3] [2,4,5]        -- [1,2,3]

{-
    6.2 Surjections, Injections, Bijections
    ---------------------------------------
    If f is a function from X to Y, there may be elements of Y that
    are not in f[X]. Functions for which this is 'not' true warrant
    special names.
    
    Surjection: if every element b in Y occurs as a function value of at
                least one a in X ie if f[X] = Y
                
    Injection:  if every b in Y is a value of at least one a in X
    
    Bijection:  if f is both injective and surjective
-}
-- given a function and its domain, determine if the function is 
-- injective
injective :: Eq b => (a -> b) -> [a] -> Bool
injective  f []     = True
injective f (x:xs) = notElem (f x) (image f xs) && injective f xs

surjective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
surjective f xs []     = True
surjective f xs (y:ys) = elem y (image f xs) && surjective f xs ys

ex622  = list2fct [(0,1), (1,0), (0,2) ]
ex622a = injective ex622 [0,1]                  -- True
ex622b = surjective ex622 [0,1] [0,1,2]         -- False

{-
    Exercise 6.23
        Implement a test for bijectivity
-}
bijective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
bijective f xs ys = surjective f xs ys && injective f xs

ex623 = bijective ex622 [0,1] [0,1,2]           -- False

{-
    Exercise 6.24
    
    Implement tests injectivePairs, surjectivePairs, bijectivePairs
-}
injectivePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> Bool
injectivePairs f xs = injective (list2fct f) xs

surjectivePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
surjectivePairs f xs ys = surjective (list2fct f) xs ys

bijectivePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
bijectivePairs f xs ys = bijective (list2fct f) xs ys

{-  
    Exercise 6.27
    
    Implement a function
        injs :: [Int] -> [Int] -> [[(Int,Int)]]
    that takes a finite domain and codomain of tyep Int and
    produces the list of all injectives from domain to codomain,
    given as lists of integer pairs.

-}
-- provided solution
injs :: [Int] -> [Int] -> [[(Int,Int)]]
injs [] xs     = [[]]
injs xs []     = []
injs (x:xs) ys =
    concat [ map ((x,y):) (injs xs (ys \\ [y])) | y <- ys ]

ex627 = injs [0,1,2] [0,1,2]

{-
    Output:
    
    *Main> ex627
    [[(0,0),(1,1),(2,2)],
     [(0,0),(1,2),(2,1)],
     [(0,1),(1,0),(2,2)],
     [(0,1),(1,2),(2,0)],
     [(0,2),(1,0),(2,1)],
     [(0,2),(1,1),(2,0)]]
    
-}
{-
    Example 6.28
    
    The bijections on a finite set A correspond exactly to the
    permutations of A. Implement a function
            perms :: [a] -> [[a]]
    that gives all permutations of a finite list. The call 
            perms [1,2,3]
    should yield:
            [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
            
    Hint: to get the permutations of (x:xs), take all the possible
          ways of inserting x in a permutation of xs.

-}
-- provided solution
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
    where
        insrt :: a -> [a] -> [[a]]
        insrt x []     = [[x]]
        insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

{-
    6.3 Function Composition
    ------------------------
    
    Suppose f:X->Y and g:Y->Z, then the 'co-domain' of 'f' 
    coincides with the 'domain' of 'g' and the composition
    of f and g is (g . f):X->Z or (g . f)(x) = g(f x)
    i.e. first apply 'f', then apply 'g' or 'g after f'
    
    (g . f) has the domain of 'f' and the co-domain of 'g'
    
    The identity function acts as a unit element for function
    composition.
    
    Suppose that f:x->Y and g:Y->Z, then
        (g . f) injective  -> f injective
        (g . f) surjective -> g surjective
        f and g injective  -> (g . f) injective
        f and g surjective -> (g . f) surjective
        
    Inverse Function
    ----------------
    A function has an inverse iff it is bijective and that inverse
    is unique.
    
    If f:X->Y, g:Y->X then (g . f) = l_x and 'g' is the 'left-inverse'
    of 'f', and 'f' the 'right-inverse' of 'g'.
    
    Every function that has a surjective right-inverse is  a biejection
    Every function that has an injective left-inverse is a biejection
    
-}        
-- Celsius to Fahrenheit, an example of a function and its inverse
c2f, f2c :: Int -> Int
c2f x = div (9 * x) 5 + 32
f2c x = div (5 * (x - 32)) 9

{-
    6.5 Partial Functions
    ---------------------
    A 'partial function' from X to Y is a function with its
    domain included in X and its range included in Y. 
    Some values of (f x) are undefined.
    In Haskell, the undefined values may produce an error
    eg \ x -> if x < 0 then error "arg out of range" else x+1
    
    but a more useful technique is to use a list
    eg \ x -> if x < 0 then [] else [x+1]
    
    or one can use the 'Maybe' datatype
    eg \ x -> if x < 0 then Nothing else (Just x+1)
    
-}
-- composing partial functions
pcomp :: (b -> [c]) -> (a -> [b]) -> a -> [c]
pcomp g f = \ x -> concat [ g y | y <- f x ]

mcomp :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
mcomp g f = (maybe Nothing g) . f

-- the maybe function allows for a variety of ways of dealing
-- with partial applications, for example, the following
-- converts a partial application to an error
part2error :: (a -> Maybe b) -> a -> b
part2error f = (maybe (error "value undefined") id) . f

{-
    Exercise 6.57
    
    Define a partial function
        stringCompare :: String -> String -> Maybe Ordering
        
    for ordering strings consisting of alphabetic characters
    in the usual order. If a non-alphabetic symbol occurs, the 
    function should return Nothing. Use isAlpha for the property
    of being an alphabetic character.
-}
-- provided solution
stringCompare :: String -> String -> Maybe Ordering
stringCompare xs ys | any (not . isAlpha) (xs ++ ys) = Nothing
                    | otherwise = Just (compare xs ys)
                    
{-
    6.6 Functions as Partitions
    ---------------------------
    Data is often classified (partitioned) by way of functions
    i.e "gender of x", "color of x", "age of x"; such 'partitions'
    have an 'equivalence' relation.
    
    The 'fct2equiv' function maps a function to the equivalence
    relation, inducing the partition that corresponds with the 
    function
    
    ex to test equality for modulo n
           fct2equiv (`rem` 3) 2 14   -> True
-}                    
fct2equiv :: Eq a => (b -> a) -> b -> b -> Bool
fct2equiv f x y = (f x) == (f y)

block :: Eq b => (a -> b) -> a -> [a] -> [a]
block f x list = [ y | y <- list, f x == f y ]

ex664a = block (`rem` 3) 2 [1..20]
ex664b = block (`rem` 7) 4 [1..20]

{-
    Exercise 6.65
    
    Implement an operation 'fct2listpart' that takes a function
    and a domain and produces the list partition that the function
    generates on the domain.
    
    Examples
        fct2listpart even [1.20]
        [[1,3,5,7,9,11,13,15,17,19], [2,4,6,8,10,12,14,16,18,20]]
        
        fct2lstpart (\ n -> rem n 3) [1..20]
        [[1,4,7,10,13,16,19],[2,5,8,11,14,17,20],[3,6,9,12,15,18]]
        
    [Note: (\\) is the 'list difference' operator ]
-}
-- provided solution
fct2listpart :: (Eq a, Eq b) => (a -> b) -> [a] -> [[a]]
fct2listpart f []     = []
fct2listpart f (x:xs) = xclass : fct2listpart f (xs \\ xclass)
    where xclass = x : [ y | y <- xs, f x == f y ]

    
