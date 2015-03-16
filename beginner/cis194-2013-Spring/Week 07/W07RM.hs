module W07RFM where

import Data.Monoid
import qualified Data.Foldable as F

{-
    Week 07 - Folds and Monoids
              Notes from Suggested Readings for Monoids
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-} 
{-
    Ref: Learn You a Haskell - Monoids
         http://learnyouahaskell.com/
            functors-applicative-functors-and-monoids#monoids
    
    Monoids
    -------
    A monoid is an associative (placement of parentheses makes
    no difference to the result) binary function that has a value
    which can act as an identity with respect to that function.
   
   For example
        * is a binary function for which 1 acts as the identity
          value since 1 * x always yields x
          
        + is a binary function for which 0 acts as the identity
          value since 0 + x always yields x
          
       ++ is a binary function for which [] acts as the identity
          value since xs ++ [] always yields xs
    
    Note that all three functions take and return values of the
    same type. There are many other binary functions in Haskell 
    that act like monoids, hence the Monoids type class.
    
    The Monoid Type Class
    ---------------------
    
    The Monoid type class is defined in Data.Monoid where it is 
    defined as:
    
        class Monoid m where  
            mempty  :: m  
            mappend :: m -> m -> m 
            
            mconcat :: [m] -> m  
            mconcat = foldr mappend mempty  
            
    From the definition we can see:
        1. only concrete types can be made monoids as the 'm' in
           the definition doesn't take any type parameters
           
        2. 'mempty' is a 'polymorphic constant' rather than a function
           as it has no parameters; it acts as the monoids 'identity 
           value'
           
        3. 'mappend' is the binary function that takes two arguments
           and returns a third; the arguments and returned value must
           all be of the same type
           
        4. 'mconcat' takes a list of values, reducing them to a single
           value by applying 'mappend' to the list elements. There is
           a supplied default definition that is usually sufficient
           for any instances.
            
    Monoid Laws
    -----------
    When defining our own monoid we have to make sure the following
    rules apply:
    
        mempty `mappend` x          = x
        x      `mappend` mempty     = x
        (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
        
    Haskell doesn't enforce these laws, we have to make sure we get
    them right.
    
    Lists are monoids
    -----------------
    The Monid instance for a list is defined as:
    
        instance Monoid [a] where  
            mempty = []  
            mappend = (++)      
    
    Note that we have to apply a specific type 'a', we can't just
    say: instance Monoid [] as monoids need concrete types.
    
    Product and Sum are Monoids
    ---------------------------
    
    Product is defined as a monoid:
    
        newtype Product a =  Product { getProduct :: a }  
        deriving (Eq, Ord, Read, Show, Bounded)  
        
        instance Num a => Monoid (Product a) where  
        mempty                        = Product 1  
        Product x `mappend` Product y = Product (x * y) 
        
    Product wraps a number value (note the constraint Num a =>)
    and can how it works in the examples given below (not that
    you'd want to multiply numbers this way). [Note that 'getProduc'
    effectively 'unwraps' the value).
    
    Sum has a similar defintion.
-}
prod1 = getProduct $ Product 3 `mappend` Product 9          -- 27                    
prod2 = getProduct $ Product 3 `mappend` mempty             -- 3
prod3 = getProduct $ Product 3 `mappend` 
                       Product 4 `mappend` Product 2        -- 24
prod4 = getProduct . mconcat . map Product $ [3,4,2]        -- 24

{-
    Any and All
    -----------
    Any and All are monoids that work with boolean values.
    Any is the logical or (||) and All is the logical and (&&)
    
        newtype Any = Any { getAny :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)  
        
        instance Monoid Any where  
            mempty = Any False  
            Any x `mappend` Any y = Any (x || y) 
        
        newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded
        
        instance Monoid All where  
            mempty = All True  
            All x `mappend` All y = All (x && y)      
            
    They work like the Product example.
-}
any1 = getAny $ Any True `mappend` Any False                    -- True
any2 = getAny $ mempty   `mappend` Any True                     -- True
any3 = getAny . mconcat . map Any $ [False, False, False, True] -- True 
any4 = getAny $ mempty `mappend` mempty                         -- False

all1 = getAll $ mempty `mappend` All True                       -- True
all2 = getAll $ mempty `mappend` All False                      -- False
all3 = getAll . mconcat . map All $ [True, True, True]          -- True
all4 = getAll . mconcat . map All $ [True, True, False]         -- False
all5 = getAll $ mempty `mappend` mempty                         -- True

{-
    The Ordering Monoid
    -------------------
    The instance is defined as
    
        instance Monoid Ordering where  
        mempty = EQ  
        LT `mappend` _ = LT  
        EQ `mappend` y = y                  -- the identity
        GT `mappend` _ = GT  
        
    Note that x `mappend` y is not the same as y `mappend` x 
    
-}
ord1 = mappend LT GT            -- LT
ord2 = mappend GT LT            -- GT
ord3 = mappend mempty LT        -- LT
ord4 = mappend mempty GT        -- GT

-- an example of using the Ordering Monoid
-- order two strings based on length, if equal lenght, order
-- alphabetically
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a  
                    
comp1 = lengthCompare "zen" "ants"          -- LT  based on length
comp2 = lengthCompare "zen" "ant"           -- GT  based on alpha order

-- but we could write lengthCompare using our knowledge of it as a
-- Monoid
lengthCompareM :: String -> String -> Ordering  
lengthCompareM x y = (length x `compare` length y) `mappend`  
                        (x `compare` y)  

compM1 = lengthCompareM "zen" "ants"        -- LT
compM2 = lengthCompareM "zen" "ant"         -- GT

{-
    Walkthrough:
            lengthCompareM "zen" "ants"
         -> mappend (compare (length "zen") (length "ants")
                    (compare "zen" "ants")
         -> mappend (compare 3 4) (compare "zen" "ants")
         -> mappend LT _
         -> LT
         
            lengthCompareM "zen" "ant"
         -> mappend (compare (length "zen") (length "ants")
                    (compare "zen" "ant")
         -> mappend (compare 3 3) (compare "zen" "ant")
         -> mappend EQ (compare "zen" "ant")
         -> compare "zen" ant"
         -> GT
                  
-}               

-- expand lengthCompareM to make the number of vowels the second
-- criterion for ordering
lengthCompareV :: String -> String -> Ordering  
lengthCompareV x y = (length x `compare` length y) `mappend`  
                     (vowels x `compare` vowels y) `mappend`  
                     (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou") 
    
compV1 = lengthCompareV "zen" "anna"        -- LT
compV2 = lengthCompareV "zen" "ana"         -- LT
compV3 = lengthCompareV "zen" "ann"         -- GT

{-
    Maybe Monoid
    ------------
    The instance is defined as
    
        instance Monoid a => Monoid (Maybe a) where  
        mempty                    = Nothing  
        Nothing `mappend` m       = m  
        m `mappend` Nothing       = m  
        Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  
        
    This instance has a class constraint; the Maybe monoid will only
    work on types that also belong to the Monoid class.
    
    If the type contents of the Maybe aren't monoids we can use
    the First a monoid to always return the (Just a) value
    
        newtype First a = First { getFirst :: Maybe a }  
        deriving (Eq, Ord, Read, Show)  
        
        instance Monoid (First a) where  
        mempty                     = First Nothing  
        First (Just x) `mappend` _ = First (Just x)  
        First Nothing  `mappend` x = x

    The mconcat function can be used to returned any (Just a)
    value form a list.
    
    There is also a Last a which works the same way, returning the
    last (Just a) value

-}    
maybe1 = Nothing `mappend` Just "andy"          -- Just "andy"
maybe2 = Just LT `mappend` Nothing              -- Just LT
maybe3 = Just (Sum 3) `mappend` Just (Sum 4)    -- Just (Sum {getSum = 7})

first1 = getFirst $ First (Just 'a') `mappend` First (Just 'b')-- Just 'a'
first2 = getFirst $ First Nothing `mappend` First (Just 'b')   -- Just 'b'
first3 = getFirst $ First (Just 'a') `mappend` First Nothing   -- Just 'a'
first4 = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10] 
            -- Just 9
            
last1 = getLast . mconcat . map Last $ [Nothing, Just 9, Just 10] 
            -- Just 10            
last2 = getLast $ Last (Just "one") `mappend` Last (Just "two") 
            -- Just "two"
            
{-
    Using monoids to fold data structures
    -------------------------------------
    The Foldable module contains implementations of foldr, foldl,
    foldr1 and foldr2 that will work on ANY type, not just lists.
    
    Where foldr has the type signature
        (a -> b -> b) -> b -> [a] -> b
        
    The foldr from Foldable has the type signature
        (Foldable t) => (a -> b -> b) -> b -> t a -> b
        
    And, as a list is just another type, Foldable foldr will also
    work on lists.
-}            
-- example of folding over Maybe
fldMaybe1 = F.foldl (+) 2 (Just 9)         -- 11
fldMaybe2 = F.foldr (||) False (Just True) -- True

-- example of making a custom type Foldable
data Tree a = Empty 
            | Node a (Tree a) (Tree a) 
    deriving (Show, Read, Eq)

-- create a Foldable instance of our Tree type
instance F.Foldable Tree where  
    foldMap f Empty        = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r 
                             
testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )                              
                             
fldTree1 = F.foldl (+) 0 testTree   -- 42
fldTree2 = F.foldl (*) 1 testTree   -- 64800

-- is any number in our tree equal to 3?
fldTree3 = getAny $ F.foldMap (\x -> Any $ x == 3) testTree  -- True

-- is any number in our tree greater than 15?
fldTree4 = getAny $ F.foldMap (\x -> Any $ x > 15) testTree  -- False

-- convert the testTree to a list
fldTree5 = F.foldMap (\x -> [x]) testTree

-- all of the above 'tricks' will work on any Foldable structure
                             
                             
