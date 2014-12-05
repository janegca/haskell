-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 9 - Types of Curried Forms and Higher Order Functions

{-
    The CURRIED FORM of functions supply less than all the
    function arguments.
    
    For example, we defined the 'remove' function as:
-}
remove :: Char -> String -> String
remove chr str = [c | c <- str, c /= chr ]

{-
    but we can create curried forms by supplying the 'Char'
    argument without the String argument
-}
removeB :: String -> String
removeB = remove ' '

removeP :: String -> String
removeP = remove '.'

removeC :: String -> String
removeC = remove ','

{-
    The type of a curried function is implied by the type of
    the full function
    
    For example,
        f :: a -> b         full function type
        x :: a              type of first argument
        f x :: b            type of curried function
        
        Here, function 'f' takes a value of type 'a' and
        TRANSFORMS it into a value of type of 'b'. Therefore,
        if 'x' has type 'a' the implied type of 'f x' is 'b'.
    
    Another example:
        f :: a -> b -> c    full function type
        x :: a              type of first argument
        f x :: b -> c       type of curried function
        
        'f' takes two values of type 'a' and 'b' and TRANSFORMS
        them into a value of type 'c'. If 'x' is a value of type
        'a' then 'f x' transforms a value of type 'b' into a
        value of type 'c'.
    
    So, what would be the type of:
    
        [remove c | c <- " ,.;:'\"?!()" ]
    
    Remember that this is a sequence whose elements are functions.
    
    Ans:
        *Main> :t [remove c | c <- " ,.;:'\"?!()" ]
        [remove c | c <- " ,.;:'\"?!()" ] :: [String -> String]
    
        A list of functions whose type is 'String -> String'
    
    REMEMBER: 'a' can be ANY TYPE and 
              LISTS can only contain ELEMENTS of the SAME TYPE
              FUNCTION TYPES ARE SEQUENCES OF TYPES
              
              So a type which is a list
              with elements which are a sequence of types must
              mean the elements are functions.
-}  
{-
    HIGHER ORDER FUNCTIONS
    
    A 'higher order function' takes another function as an argument
    and/or returns another function as a result.
    
    The function composition operator (.) is a higher order function
    that both takes functions as arguments and returns a function
    as its value.
    
    The functions supplied as arguments can transform any type into
    any other type BUT they MUST BE COMPATIBLE for function composition.
    
    THE RIGHT-HAND OPERAND MUST DELIVER A VALUE OF A TYPE THAT CAN
    BE TRANSFORMED BY THE LEFT-HAND OPERAND.
    
    Which makes the (.) type look a little tricky:
    
        *Main> :t (.)
        (.) :: (b -> c) -> (a -> b) -> a -> c
        
    This is the first time we've seen parenthesized values in a
    type signature. Because the right-arrow (->) is RIGHT-ASSOCIATIVE
    a sequence of types 
            a -> b -> c -> d
    would normally be grouped as
            a -> (b -> (c -> d))
    so function types MUST BE enclosed in parentheses to define the
    correct order of type evaluation. 
    
    Going back to our (.) type signature, we can insert the implied
    parentheses:
    
            (b -> c) -> (a -> b) -> (a -> c)
            
    and we can think of this in terms of 'curried invocations':
    
        (.) (b -> c)  would take a type 'b' and transform it
                      into a type 'c' 
                      
    so we know 'x' (any value we pass to our curried function) must
    be a value of type 'b' and this is what our second argument
    (a -> b) produces. 
    
        (.) (b -> c) (a -> b)  would take a value of type 'a'
                               transform it to a value of type 'b'
                               and then transform it into a value
                               of type 'c'
    
    which means any value of 'x' must be of type 'a' which will be
    transformed into type 'b' which will be transformed into type 'c'
    
    Only in this particular instance rather than directly returning
    a value of type 'c' we are returning a function that will take a
    type 'a' and transform it into a type 'c'.
    
    In a sense, the function composition operator creates a 'black-box'
    the final function we see is of the form: a -> c; we give it a value
    of type 'a' and it is magically transformed, through a series of
    steps we, as users of the function, don't see, into a value of 
    of type 'c'.
    
    We can actually think of the entire type declaration as a 'curried
    invocation'
    
       f ((b -> c) -> (a -> b) -> (a -> c)) x
       
    where the first argument is the sequence of functions
    to be applied to the second argument, 'x', with the type of 
    'x' being implied by the argument of the first function it
    will encounter during evaluation: (a -> c). So, the type of any 
    value 'x' we pass in MUST BE of type 'a'. 
    
    Can also think of it as:
    
        f :: b -> c
        f b = c                 -- transfrom type 'b' to type 'c'
        
        g :: a -> b             -- transform type 'a' to type 'b'
        g a = b
        
        h :: a -> c             -- transfrom type 'a' to type 'c'
        h x = f( g(x) )         -- black box; 'x' must be type 'a'
            = c                 -- final value of type 'c'
            
    NOTE: If a function has the signature: f :: a -> a -> a
          then all the values, both the arguments and the returned value,
          MUST HAVE THE SAME TYPE and that type will necessarily be the
          type of the first argument.
          ie in f x y,  the type of 'y' MUST BE the same as the type
             of 'x'
          
          However, a function having the signature: f:: a -> b -> c -> d
          DOES NOT NECESSARILY HAVE TO HAVE VALUES OF DIFFERENT TYPES
          they could simply represent different VALUES of the same type.
          
          For example, the function 'removeBP', formed
          by combining removeB and removeP: 
                
           (String -> String) -> (String -> String) -> String -> String
                
          has a type of: String -> String
-}
removeBP :: String -> String
removeBP = removeB . removeP  

{-
    Note that the order in which we combine functions with the
    (.) operator is irrelevant. For example,  a composition of
    the following type:
    
        (a -> b) -> (c -> a) -> (c -> b)
        
    is equivalent to all of these compositions:
         -    =      *    -      *    =
        (b -> c) -> (a -> b) -> (a -> c)
        (a -> c) -> (b -> a) -> (b -> c)
        (c -> a) -> (b -> c) -> (b -> a)
        
    In each case, the types in the positions corresponding to the 
    symbols: - = * all agree.
        
-}

-- Other examples
f :: Char -> String
f chr = chr : []            -- transform a character to a string

g :: String -> [String]
g str = [ str ]             -- transform string to nested list

-- what is the type of g . f?
res1 = g . f

-- given the type of f1 below
f1 :: String -> String -> Bool
f1 a b = a == b

-- what is the type of the curried invocation f "x"?
fx = f1 "x"


