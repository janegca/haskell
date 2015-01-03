-- a type class in Haskell is a bit like an interface in Java
--(think of 'a; as a type-parameter, like the Foo in 
--    "public class Blah implements Comparable<Foo>")

-- the below is simplified, but valid
-- Notes:
--      functions in classes are called 'methods'
--      methods are defined, BUT NOT implemented, in classes
--
--      any type which is part of the class will have have a
--      corresponding 'instance' which does implements all the methods
--      defined in the class
--
--      types which have a class instance are MEMBERS of the class
--
--      Classes declare methods which can 'process' data of certain
--      'types', namely, those 'types' which provide an 'instance'
--      for the class methods 
--
--      an instance defines the way in which the data 
--      structured by the type is to be processed by the methods 
--      declared in the associated class
--
--      Types can be members of multiple 'classes', for example,
--      the Integer type is a member (provides an 'instance') for
--      each of the following 'type classes': 
--          Enum, Eq, Integral, Num, Ord, Read, Real, Show
--     
{-

    *Main> :info Integer
    data Integer
      = integer-gmp:GHC.Integer.Type.S# GHC.Prim.Int#
      | integer-gmp:GHC.Integer.Type.J# GHC.Prim.Int# GHC.Prim.ByteArray#
        -- Defined in ‘integer-gmp:GHC.Integer.Type’
    instance Enum     Integer -- Defined in ‘GHC.Enum’
    instance Eq       Integer -- Defined in ‘integer-gmp:GHC.Integer.Type’
    instance Integral Integer -- Defined in ‘GHC.Real’
    instance Num      Integer -- Defined in ‘GHC.Num’
    instance Ord      Integer -- Defined in ‘integer-gmp:GHC.Integer.Type’
    instance Read     Integer -- Defined in ‘GHC.Read’
    instance Real     Integer -- Defined in ‘GHC.Real’
    instance Show     Integer -- Defined in ‘GHC.Show’

-}

class Mathy a where
  add   :: a -> a -> a  
  times :: a -> a -> a
  
-- now we can make data types that are instances of Mathy, meaning they 
-- have to implement the functions that Mathy specifies
-- [Note: 'instances' provide implementation of the declared methods
--        in Haskell, a 'class' is declared over functions vs objects
--        there is only one class instance per type (vs multiple instances 
--        of objects of the same class) and members of a class all share
--        the same 'instance' i.e. have same behaviours (methods)]

data Vector = Vector Int Int Int  deriving Show

instance Mathy Vector where
    add (Vector a b c) (Vector x y z)   = Vector (a+x) (b+y) (c+z)
    times (Vector a b c) (Vector x y z) = Vector (a*x) (b*y) (c*z) 
        -- this isn't real vector multiplication; just for demo

-- examples
v1 = Vector 1 1 1
v2 = Vector 2 2 2

{-
    [1 of 1] Compiling Main             ( 21-typeClasses.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> v1
    Vector 1 1 1
    *Main> v2
    Vector 2 2 2
    *Main> add v1 v2
    Vector 3 3 3
    *Main> times v1 v2
    Vector 2 2 2
    *Main> 

-}



