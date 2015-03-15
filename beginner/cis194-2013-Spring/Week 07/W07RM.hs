module W07RFM where

import Data.Monoid

{-
    Week 07 - Folds and Monoids
              Notes from Suggested Readings for Monoids
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-} 
{-
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
prod1 = getProduct $ Product 3 `mappend` Product 9 
prod2 = getProduct $ Product 3 `mappend` mempty
prod3 = getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
prod4 = getProduct . mconcat . map Product $ [3,4,2]

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

-}