-- here we give our class a type parameter
class Useless a where
    func :: a -> String
    func arg = "default value"  -- we also give it a (not-very-helpful) 
                                -- default implementation

data Thing = Thing String Int
instance Useless Thing     -- since we have a default func, we don't have 
                           -- to specify one

-- lastly, there are a lot of built in type classes
data Pair = Pair Int Int

-- An example is Show; things that are instanced of show
-- can be turned into Strings

-- a quick example of using Show (the method show forces things to 
-- implement is called "show")
-- notice that the numbers are instances of Show and we use that to turn 
-- them into strings here
pairToString (Pair x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

-- now that we have a function to turn pairs into strings we can actually
-- just make pair an instance of Show
instance Show Pair where
    show = pairToString

-- now we could say
turnMyPairIntoAString pair = show pair

-- or
aPairAsAString = show (Pair 23 14)

{-

    [1 of 1] Compiling Main          ( 22-moreTypeClasses.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> func (Thing "hi" 1)
    "default value"
    *Main>     
    *Main> Pair 4 6      -- works we provided a Sho instance
    (4,6)
    *Main> turnMyPairIntoAString (Pair 3 5)
    "(3,5)"
    *Main> aPairAsAString
    "(23,14)"
    *Main> 

-}
-- Note:  takes a little getting used to, want to write myPair.show
--        but the method 'show' DOESN'T belong to the type structure
--        'Pair' ... it's a behaviour that will work when 'applied'
--        to a Pair structure ... ie it's a method that can PROCESS
--        data structured as a Pair