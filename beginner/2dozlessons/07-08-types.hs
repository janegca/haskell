-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 7 Types
-- Chapter 8 Function Types, Classes, Polymorphism

{-
    The idea of types is a 'fundamental, organizing principle in
    software construction'
    
    You can use the ':type' (':t') command in ghci or Hugs to check the
    type of a formula
    
    Example
        Prelude> :t reverse "abc"
        reverse "abc" :: [Char]   
        
        The double-colon (::) is read as 'has type'
        
        Note that the data type 'String' is a 'type synonym' for a
        [Char] (a list of elements of type Char]
        
    It we look at the type of the reverse function (without any
    argument) we get:
    
            Prelude> :t reverse
            reverse :: [a] -> [a]    

            'reverse' takes an argument of type [a]
            and TRANSFORMS (->) it to another value of type [a].
            
            'a' here is a 'type variable' and stands for 'any type'
            so 'reverse' will work on any list, regardless of the
            type of the contained element
            
    The 'reverse' function is said to be POLYMORPHIC because it can
    work on many different types of elements i.e. it can take on
    many forms.
    
    Polymorphism allows us to use a single function in multiple contexts; 
    without this ability we would need to write 'reverse' for every
    possible type of list i.e. reverseChar, reverseBool, reverseIntegers,
    etc. 
-}
{-
    Function types are characterized by their argument and result
    types. i.e. 
        THE TYPE OF A FUNCTION IS AN ORDERED SEQUENCE OF TYPES.
        
    All functions, including the ones we write, have a type.
    For example,
        isPalindromic :: [Char] -> Bool
        allLowerCase  :: [Char] -> [Char]
        
    If we look at the type of 'isPalindrome' we'll see something 
    slightly different:
    
        *Main> :t isPalindrome
        isPalindrome :: Eq a => [a] -> Bool    
        
    The 'Eq a =>' is a TYPE CONSTRAINT. It means that while the
    elements of the argument list [a] can be of any type 'a',
    THE 'TYPE' ITSELF MUST BE IN THE EQUALITY CLASS 'Eq'.
    
    A CLASS IS A COLLECTION OF TYPES that share a collection of
    functions and/or operations.
    
    For the 'isPalindrome' function to work, the elements in the
    argument list must be comparable i.e. we need to be able
    to write 'str1 == str2' and get back a valid result: True or False.
    
    Note that operators, which are 'conceptually equivalent' to functions,
    also have types:
    
        *Main> :t (==)
        (==) :: Eq a => a -> a -> Bool        
        
    Another common type constraint is 'Ord a =>'; types in this class
    can be compared using <=, <, >, >= operators; values of types in
    the Ord class know if they are greater or less than other values
    of the same type. For example, values in the Char, 'a', 'b', 'e'
    know whether they come before or after other Char's.
-}
res1 = 'z' <= 'a'
res2 = 'z' > 'a'
res3 = '+' >= '-'

{-
    TYPE DECLARATIONS
    
    While Haskell can deduce the type of a function we write, it is
    better to tell Haskell through the use of a TYPE DECLARATION.
    For example, rather than simply writing a function such as
    
        remove chr str = [ c | c <- str, c /= chr]
        
    We would define it as
    
-}
remove :: Char -> String -> String
remove chr str = [ c | c <- str, c /= chr ]

{-
    The 'type declaration' is:
    
        remove :: Char -> String -> String
    
    It specifically tells the Haskell compiler that our function
    takes a Char and a String and returns a String. 
    
    The compiler will still try to deduce the function
    type (either a generalized or specialized version) and if it
    can't come up with one that matches our type declaration it
    will throw an error. This forces us to carefully think through
    the types our function is using and creating which in turn
    helps us to a better understanding of the problem we're trying
    to solve which, hopefully, leads to improved formulas.
-}
