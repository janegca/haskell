{-
    Week 10 Applicative Functors - Part 1
        Notes and exercises
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        10-applicative.html
        
-}
import Control.Applicative

type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

{-
    Given the Name and Employee definitions (shown above) we can see
    that the Employee constructor has the type:
    
        Employee :: Name -> String -> Employee
        
    so, if we have a Name and a String we can apply the Employee
    constructor to create an Employee but say we have
        Maybe Name, Maybe String
    or  [Name], [String]
    or  (e -> Name) (e -> String)
    
    We can still create Employee's by writing functions like
    
        (Name -> String -> Employee) 
     -> (Maybe Name -> Maybe String -> Maybe Employee)
     
        (Name -> String -> Employee)
     -> ([Name] -> [String] -> [Employee])
     
        (Name -> String -> Employee)
     -> ((e -> Name) -> (e -> String) -> (e -> Employee))
    
    This pattern generalizes into:
    
        (a -> b -> c) -> (f a -> f b -> f c)
        
    which looks very much like the type signature of fmap:
    
        fmap :: (a -> b) -> (f a -> f b)
        
    the difference is the extra argument. fmap gives us a way
    to apply values in a context but what we need is a way
    to apply functions in a context to values that are also
    in a context; which brings us to the ..
    
    Applicative type class
    ----------------------
    
        class Functor f => Applicative f where
          pure  :: a -> f a
          (<*>) :: f (a -> b) -> f a -> f b   

    pure allows us to 'inject' a value of type 'a' into a container
         for situations where we want to apply some function to arguments 
         in the context of some functor f, but one or more of the 
         arguments is not in f
         
    <*> operator is 'ap' (short for apply)         

    defines <$> as a synonym for fmap and the functions
    
        liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
        liftA2 h fa fb = h <$> fa <*> fb
        
        liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
        liftA3 h fa fb fc = ((h <$> fa) <*> fb) <*> fc
        
    Note that both <*> and <$> are left-associative (infixl 4)
    
    Rather than calling liftA2 and liftA3, usually use the pattern
    
        f <$> x <*> y <*> z <*> ...
        
    Laws
    ----
        the most important law is:
            
            f `fmap` x == pure f <*> x
            
        ie mapping a function 'f' over a container 'x' should have
           the same result as first injecting the function into
           the container and then applying it to x with <*>
    
    
-}
                
{- Examples

    The Maybe instance of Applicative is defined as:

        instance Applicative Maybe where
          pure              = Just
          Nothing <*> _     = Nothing
          _ <*> Nothing     = Nothing
          Just f <*> Just x = Just (f x)
-}  
m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

-- apply the constructor Employee using the Maybe values as input
ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = Employee <$> m_name2 <*> m_phone2
  
{-
    What's going on here? The constructor Employee is 'lifted'
    
        Employee <$> m_name2             <*> m_phone2
     -> (Employee <$> Just "Brent")      <*> m_phone2
     -> (fmap Employee Just "Brent")     <*> m_phone2
     -> Just (Employee "Brent")          <*> m_phone2
     -> Just (Employee "Brent") <*> Just "555-1234"
     -> Just (Employee "Brent" "555-1234")
     -> Just (Employee {name = "Brent", phone = "555-1234"})

-}  