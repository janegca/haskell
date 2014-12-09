-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 24 - Algebraic Data Types

{-
    Creating custom type classes
    
    Below is the definition of a custom type class, 'Color'
    It can take one of 3 values: Red, Green or Blue
    The values are type 'constructors' for the type 'Color'
    Constructor names MUST begin with a Capital letter
    
    We also want the type to inherit some basic behaviours,
    we do this by using the 'deriving' clause; it lists the
    other types want our custom type to belong to and the
    compiler will automatically create the expected behaviours.
    
    Here, we want Color to be comparable, orderable, enumerable
    and, last, we want to be able to display its value.
    
    Note that the values, for comparison purposes, will be
    in accordance with their declared order, so 
        Red < Yellow < Blue
        
    Ex
    *Main> Red < Blue
    True
    *Main> Blue > Yellow
    True

    *Main> [Red .. Blue]
    [Red,Yellow,Blue]

    
-}

data Color = Red | Yellow | Blue
    deriving (Eq, Ord, Enum, Show)

{-
    Another custom type we might want, 'Figure' is defined below
    A Figure type can be one of two values: Circle or Rectangle
    The Circle constructor has two parameters (fields)
        Color   - the color of the circle
        Double  - the circle radius
     
    The Rectangle constructor has 3 parameters (fields)
        Color   - the rectangle color
        Double  - the rectangle length
        Double  - the rectangle width
        
    NOTE: The Figure class does not derive from 'Enum'
          Only class types having NO FIELDS can inherit from the
          Enum type class
        
    eg
        *Main> circle
        Circle Red 1.0
        *Main> rect
        Rectangle Blue 5.0 2.5
        *Main> otherCircle
        Circle Yellow 3.141592653589793
    
-}
data Figure = Circle Color Double
           | Rectangle Color Double Double
    deriving (Eq, Show)
           
circle = Circle Red 1
rect   = Rectangle Blue 5 2.5
otherCircle = Circle Yellow pi

