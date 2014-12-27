-- Informatics 1 - Functional Programming 
-- Lecture 13-14 - Type Classes
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  10/11/2014  33:00 minute mark
--
-- Other references used:
--      RWH ('Real World Haskell') Chapter 2
--          http://book.realworldhaskell.org/read/types-and-functions.html
--      LYAHFGG ('Learn You a Haskell for Great Good') Types and Type Classes
--          http://book.realworldhaskell.org/read/types-and-functions.html
--      'The Craft of Functional Programming', Addison-Wesley
--      'Programming in Haskell' by Graham Hutton, Cambridge Univ. Press
--      'The Haskell School of Expression' by Paul Hudak, Cambridge, 
--          University Press
--      Hakell WikiBook
--          https://en.wikibooks.org/wiki/Haskell/Classes_and_types
--
--      Haskell 2010 Language Report
--      https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2
--
{-
    Declaration for the 'elem' function
    
        elem :: Eq a => a -> [a] -> Bool
        
    'Eq a =>' is a CONSTRAINT on the types of data that can be used
    by the formula
    
    The function itself HAS TO COMPARE ELEMENTs, regardless of how 
    it is implemented
    
        -- comprehension
        elem x ys = or [ x == y | y <- ys ]

        -- recursion
        elem x [] = False
        elem x (y:ys) = x == y || elem x ys

        -- higher-order
        elem x ys = foldr (||) False (map (x ==) ys)

    So the data MUST be comparable; and this is what the 'Eq a =>'
    constraint declares.
-}
{-
    Definition of Equality Type Class - creates 'instances' of various
    types, defining 'how' members of the type are 'equal'
    
        class Eq a where
        (==) :: a -> a -> Bool      -- type signature
        
        instance Eq Int where       -- implementation for type Int
        (==) = eqInt                -- eqInt is a machine instruction
        
        instance Eq Char where      -- implementation for type Char
        x == y = ord x == ord y
        
        instance (Eq a, Eq b) => Eq (a,b) -- implementation for a pair
        where
            (u,v) == (x,y) = 
                       (u == x) && (v == y)
        
        instance Eq a => Eq [a] where   -- implementation for a List
        [] == []     = True
        [] == y:ys   = False
        x:xs == []   = False
        x:xs == y:ys = (x == y) && (xs == ys)    
-}
{-
    Haskell Type System (see RWH Ch 2)
    
        - types are 'strong, static and can be automatically inferred'
        
        Strong
            all expressions must be 'well typed' i.e. the types used
            in the expression must have the properties used/referenced
            in the expression
            
            the compiler will not implicitly coerce (cast, convert) one
            type to another; if a passed value's type doesn't agree with 
            a function's parameter type an error will be thrown

        Static
            the compiler knows the type of every value at compile time
        
        Type inference
            the compiler can deduce the type of almost all expressions
            during compile time
            
            if the inferred type differs from the declared type 
            signature, an error will be thrown
            
            [Note: type signatures are really a form of documentation
                   they let us, and other programmers, know what we
                   our code is trying to do]
                   
        Strong and static typing makes it impossible for type errors
        to occur at runtime; they are all caught during compile
        
-}
{-
    TYPES (see LYAHFGG)
    
    - everything in Haskell has a type
   
   - the compiler can 'infer' the type of every expression
        
    - explicit type names ALWAYS begin with a capital letter
        eg Char, Bool, Int, Float, etc.
    
    - functions have types [Note: functions in Haskell are values, and
      can be treated very similar to data values] Parameters are separated
      by arrows (->) and there is no distinction between the parameters
      and the return type]
      eg
        removeNonUppercase :: String -> String
        
        declares removeNonUppercase takes a String as an argument
        and returns a String
        
        addThree :: Int -> Int -> Int -> Int
        
        declares 'addThree' takes thee Int arguments and returns
        ans Int
    
    TYPE VARIABLES 
    
    - you can easily check the type of any value (function or otherwise)
      in ghci by typing ':type' or ':t'
      eg  to find the type of the list function 'head'
      
            Prelude> :t head
            head :: [a] -> a
            Prelude>      
       
    - the 'a' is a 'type variable', it signals the value in question
      can have 'any type'
      
    - functions that have 'type variables' rather than 'specific types'
      are called 'polymorphic' or 'overloaded' functions' 
      i.e. they can handle 'many forms'
      
      [Note: 
        polymorphic functions can handle values of many different
        types in the same way a List can hold elements of any type
        
        For example, every non-empty list will have a 'first' or 'head'
        element that can be returned; the function 'head' is NOT
        concerned with the type of the element but only with it's 
        position in the list.
        
        If the function must rely on a particular type property,
        the function type signature will contain a 'constraint' to
        signal that fact. For example. the type signature of the
        square root function is:
        
                Prelude> :t sqrt
                sqrt :: Floating a => a -> a
                
        The 'Floating a =>' is included in the type signature as 
        a 'constraint', it signals that the function can only work with
        'Floating' point numbers; if any value of another type is
        passed to it the compiler will throw an error.]
        
    
    TYPE CLASSES [Note: 'Class' DOES NOT EQUAL OOP 'Class'!!!]
        
    - Haskell has 'type classes' that define basic properties which
      are shared by all members of the same class i.e. any type that
      declares itself to be a member of the type class implements or
      supports all the behaviours of the type class  

    - some basic type classes are: 
        Eq, Ord, Show, Read, Enum, Bounded, Integral, Floating        
          
    - nearly all the provided types are members of Eq, Ord, Show
    
    [NOTES:
     'The Craft of Functional Programming' describes a type class
     as 'the collection of types over which a function is defined'
     
     'Type Classes in Haskell' says that 'classes provide a uniform
     solution to overloading'. 
     
     A 'class' declaration provesd the names and type signatures of the 
     class operations.
     e.g. 
            class Eq a where
                (==) :: a -> a -> Bool
                
            declares that the type 'a' belongs to class 'Eq' if there
            is an operation (==) of type a -> a -> Bool implemented
            by the type
            
    An 'instance' declaration provides a 'method' that implements
    each class operation for a given type.
    e.g.
            instance Eq Int where
                (==) = primIntEq
                
            declares that the type 'Int' belongs to the class Eq
            and that the type of equality (==)  between
            values of type Int is given by the (machine) function
            'primEqInt' which must have the type Int->Int->Bool
    
    To see the class and instance declarations for a class, type
    :info or :i in ghci
    
    Example: 
        To see they class declaration, class operation signatures
        and types which are members (instances) of the class Num
    
        Prelude> :i Num
        class Num a where
          (+) :: a -> a -> a
          (*) :: a -> a -> a
          (-) :: a -> a -> a
          negate :: a -> a
          abs :: a -> a
          signum :: a -> a
          fromInteger :: Integer -> a
            -- Defined in ‘GHC.Num’
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Int -- Defined in ‘GHC.Num’
        instance Num Float -- Defined in ‘GHC.Float’
        instance Num Double -- Defined in ‘GHC.Float’
    ]    

    CLASS INHERITANCE
    
    Classes can 'inherit' from other classes, the class declaration
    for 'Ord' is:
    
        class Eq a => Ord a where
          compare :: a -> a -> Ordering
          (<)  :: a -> a -> Bool
          (>=) :: a -> a -> Bool
          (>)  :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          max  :: a -> a -> a
          min  :: a -> a -> a
            
        which declares that any member of class 'Ord' MUST ALSO BE
        a member of class 'Eq'
    
-}
{-  
    CUSTOM TYPES
    
    You can create your own types by using: type, data or newtype.
    
    The 'type' mechanism allows you to create synonyms or aliases
    of existing types
    eg
        Point  = (Int, Int)
        Radius = Float
        
        these lines declare that a Point type will be treated
        exactly the same as a pair of Int's and a Radius type
        will be treated exactly the same as a Float type
        
    The 'data' mechanism allows you to define types with 
    constructors. These are truly 'new' types
    eg
        data TrafficLight = Red | Green | Yellow
        
        declares a TrafficLight may only have one of three
        values: Red, Green or Yellow [Note that Red, Green
        Yellow are 'constructors' and NOT values. You can
        pattern match on them but you can not equate them
        to an rgb color or an int value or any other value.]
        
        data C = F Int Int Bool
        
        declares a type 'C' with one constructor, F that
        takes three arguments of type Int, Int and Bool.
        
    You can also create 'labelled fields' for the constructors
    For example,
         data S = S1 { x :: Int } | S2 { x :: Int }
         
         gives each of the arguments to the constructors
         S1 and S2 the name 'x'; note that 'x' refers to
         a different value in each case (you cannot give
         the same name to different arguments in the same
         context. i.e. S1{ x::Int, x::Float} would produce
         and error.
         
         data C = F { f1,f2 :: Int, f3 :: Bool } 
         
         gives each of the 3 argument's required by the 'C'
         constructor 'F', names
        
    
    'newtype' is similar to 'data' EXCEPT it is confined
    to one constructor; like 'data' it can have labelled fields
    
    'newtype' is similar to 'type' as it 'renames' an existing type; 
    EXCEPT it explicitly creates a new type that has the same 
    representation of the type it renames. You can define new
    instances for a 'newtype' but not for a 'type'
        
    eg
        newtype Age = Age { unAge :: Int }
        
        brings into scope both a constructor (Age) and a
        de-constructor (unAge)
        Age :: Int -> Age
        unAge :: Age -> Int
-}

