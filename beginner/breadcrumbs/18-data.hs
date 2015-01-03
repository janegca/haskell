-- This makes a Vector type that has three Int-type fields
data Vector = ConstructorName Int Int Int
    deriving Show

 -- the constructor name can be the same as the type name if we want
data Vec = Vec Int Int Int deriving Show

-- we can also have multiple constructors
-- note that the constructor really determines a lot more about
-- the structure than a "constructor" does in C++/Java
data PolyVec = R2 Int Int     -- R2 is the set of points in the plane, 
                              -- but we're using it as the name of a 
                              -- constructor for 2d vectors
             | R3 Int Int Int -- R3 is 3d space
             | R4 Int Int Int Int  
    deriving Show


-- we can pattern match on data types to extract their fields
-- also, notice how on the left (ConstructorName x y z) is a pattern match
-- while on the right, it's actually calling the constructor like a 
-- function to return a new Vector
vectorPlus (ConstructorName a b c) (ConstructorName d e f) 
    = (ConstructorName (a+d) (b+e) (c+f))
    
-- [Note: could do the same with Vec]
vecPlus (Vec a b c) (Vec d e f) = (Vec (a+d) (b+e) (c+f))    

-- this function is identical to the constructor function that the data 
-- statement generates for us
makeVector x y z = ConstructorName x y z

-- we could even write [in point-free style]
makeVector' = ConstructorName

-- lastly an example of pattern matching on PolyVec constructors
project (R2 x y)   = R3 x y 0
project (R3 x y z) = R4 x y z 0

-- Notes: example vectors
v1 = makeVector 1 1 1
v2 = makeVector 2 2 2
v3 = Vec 3 3 3
v4 = Vec 4 4 4

pv1 = R2 1 1
pv2 = R3 2 2 2

{-
    [1 of 1] Compiling Main             ( 18-data.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> vectorPlus v1 v2
    ConstructorName 3 3 3
    *Main> vecPlus v3 v4
    Vec 7 7 7
    *Main> project pv1
    R3 1 1 0
    *Main> project pv2
    R4 2 2 2 0
    *Main>     

-}
