{-
    Week 12 Monads
        Notes from readings
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/12-monads.html

    Reading: Typeclassopedia
    https://wiki.haskell.org/Typeclassopedia#Monad
    
-}
{-
    A Monad is a type class defined as:
    
        class Monad m where
          return :: a -> m a
          (>>=)  :: m a -> (a -> m b) -> m b
          (>>)   :: m a -> m b -> m b
          m >> n = m >>= \_ -> n        -- default implementation of >>
         
          fail   :: String -> m a    

    where
        return      wraps a value in a monad
        (>>=)       'bind', sequentially compose two actions passing
                    the result of the first as an argument to the second
        (>>)        sequentially compose two actions, discarding the
                    any value produced by the first; has a default
                    implementation
        fail        to wrap an error string?? 
                   (Yorgey calls it a hack that shouldn't be in Monad)
                    
--}