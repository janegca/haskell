{-
    Chapter 6 - Functions
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
import Data.List

{-
    6.1 Basic Notions
    -----------------
        function  - transforms one given object into another
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
    y in ran(f) for which (x,y) in f. Where the domain: dom(f) of
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

-- if a function is defined on an enumberable domain, we can list
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
        'co-domain of f'
        
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