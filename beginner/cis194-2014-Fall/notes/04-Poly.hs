{-
    CIS194 - Week 4 Parametric Polymorphism
    Source: http://www.seas.upenn.edu/~cis194/lectures/04-poly.html
    
    Topics
        Polymorphic Data Types
        Polymorphic Functions
        Parametric polymorphism
        Total and partial functions
            Replacing partial functions
            Writing partial functions
-}
{-
    MAYBE
    
    Prelude includes the following definition:
    
        data Maybe a = Just a
                     | Nothing
                     
    where,
        Maybe   is a 'type constructor' or 'parameterized type'
        a       is a 'type variable'
        
    Maybe is a generic version of the MaybeLogMessage and MaybeInt
    types used in Week 3 homework
-}
data LogMessage = LogMessage Int String

data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM
                     
data MaybeInt = ValidInt Int
              | InvalidInt
              
example_a :: Maybe Int -> Int
example_a (Just n) = n
example_a Nothing  = (-1)

example_b :: LogMessage -> Maybe String
example_b (LogMessage severity s) | severity >= 50 = Just s
example_b _                                        = Nothing

ex1 = example_a (Just 5)
ex2 = example_a Nothing
ex3 = example_b (LogMessage 10 "some text")     -- Nothing
ex4 = example_b (LogMessage 200 "error text")   -- Just "error text"

{-
    With 'Maybe' we are substituting 'types' into 'a'. 
    Here's another example with lists.
    
    Given a type t, a (List t) consists of either the constructor 
    Empty, or the constructor Cons along with a value of type t and 
    another (List t). Here are some examples:
-}
data List t = Empty 
            | Cons t (List t)
        deriving Show

lst1 :: List Int
lst1 = Cons 3 (Cons 5 (Cons 2 Empty))

lst2 :: List Char
lst2 = Cons 'x' (Cons 'y' (Cons 'z' Empty))

lst3 :: List Bool
lst3 = Cons True (Cons False Empty)

{-
    Note that [type] is really just syntactic sugar for '[] type'.
    
    'Maybe' and 'List' are examples of 'polymorphic structures'.
    We can also create 'polymorphic functions'.
    
    Say, for example, we want to return something even if our
    list is empty but we don't know what type the list has;
    we simply use the type variable 'a' which allows GHC to
    use 'type inference', ie GHC will use the type of whaever
    list it is given
    
-}
safeHead :: List a -> Maybe a
safeHead Empty      = Nothing
safeHead (Cons x _) = Just x

{-
    With polymorphic functions THE CALLER GETS TO PIC THE TYPE
    i.e. the type of the passed argument determines the type
         used in the function
         
    The 'isJust' function below will always work BECAUSE it
    doesn't care what type 'Just' is
         
-}
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

{-
    ALL HASKELL FUNCTIONS MUST BE PARAMETRIC IN THEIR TYPE
    PARAMETERS i.e. they MUST NOT CARE about the type of their
    parameters (no selecting on TYPE to determine behaviour
    i.e. no 'instanceOf' or 'typeOf' decisions). 
        
    Consequences of 'parametricity':
    type erasure - type info is dropped at compile time as it
                   is never needed with a function
                   as a result, code can be up to 20x faster
                   than Python, which needs to keep type info
                   
    limits functions you can write
        i.e. strange :: a -> b
             is impossible, you cannot test 'a'ny type and convert
                it to any other type 'b'
                
             limited :: a -> a
             must always return the same type it received
                limited x = x
    
    Examining the type of a function thus gives you clues as to
    what the function does
-}
{-
    TOTAL AND PARTIAL FUNCTIONS
    
    Consider the polymorphic type: [a] -> a
    The type says, given a list of things of type a, produce
    some value of type a. What functions can have such a type?
    'head' is one example. What happens if 'head' is given an
    empty list as input?
    
    Source code for 'head' from Prelude:
    
    -- | Extract the first element of a list, which must be non-empty.
    head                    :: [a] -> a
    head (x:_)              =  x
    head []                 =  badHead
    
    If given an empty list IT CRASHES! It can do nothing else AS IT
    MUST WORK ON ALL TYPES, there is no way to make up an arbitrary
    type for it to return.
    
    'head' is a PARTIAL FUNCTION - for certain inputs, IT WILL CRASH
    Recursive functions that might run forever are also PARTIAL FUNCTIONS
    
    TOTAL FUNCTIONS are functions that are well defined on all inputs.
    ie TOTAL FUNCTIONS RETURN VALID RESULTS FOR EVERY POSSIBLE
       ARGUMENT
    
    Try to AVOID PARTIAL FUNCTIONS.
    
    'head' along with 'tail, init, last, and (!!)' ARE MISTAKES; they
    are all partial functions and should NOT have been included in 
    Prelude. DO NOT USE THEM!
-}
{-
    REPLACING PARTIAL FUNCTIONS
    
    Replace partial functions by functions that use pattern-matching.
    Consider the following two functions 'doStuff1' and 'doStuff2'
    Both use pattern matching and are 'total functions' BUT only
    the second one is 'obviously' total
-}
doStuff1 :: [Int] -> Int
doStuff1 []     = 0
doStuff1 [_]    = 0
doStuff1 xs     = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []      = 0
doStuff2 [_]     = 0
doStuff2 (x:y:_) = x + y

{-
    WRITING PARTIAL FUNCTIONS
    
    What if you find yourself writing a partial function?
    First, change the output type to indicate it could fail
    (possibly by using Maybe) as in 'safeHead' above.
    
    What if you KNOW you will never call the function with
    an empty list? If that condition is guaranteed then
    the type should reflect. the guarantee.
    
    For example
    
-}
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs  -- see Note below for '$'

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as

{-
    NOTE on '$'
        See: https://www.haskell.org/haskellwiki/$
        
        an infix operator that applies the function on it's
        left to the value on it's right
        Has the lowest right-associative binding precedence
        
        Ex  f g h x     = ((f g) h ) x    -- normal binding
            f $ g $ h x = f( g (h x))     -- binding precedence changed
            
        Essentially, f $ x = f x  -- everything to the right becomes
                                     a value to which f is applied
-}
{-
    Some properties are more complex to code than others; Haskell
    does have a way to create 'dependent types' which, in general,
    can enforce arbitrary properties in types but they are beyond
    the scope of the course.
-}



