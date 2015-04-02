{-
    Week 12 Monads
        Notes from readings
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/
            lectures/12-monads.html

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
                    
    Monad Instances
    ---------------
        
    Maybe   - models failures; if any action in the chain fails,
              i.e. produces a Nothing value, the series of bound
              actions fail
              
    IO      - The IO monad is a special case, it contains a main method 
              which the compiler passes to the runtime. The 'do' notation
              is syntactic sugar for writing out sequential actions

    ((->) e)- Control.Monad.Reader provides Reader a e which is a 
              wrapper for ((->) e) along with a Monad and utility
              functions ask, asks and local used to read environment
              values
              
    Writer  - Control.Monad.Writer allows information to be collected
              as a computation progresses; for Writer a w, a and w are
              both monads; 'a' is the output value and 'w' is an 
              annotation or 'log' of what occurred that can be accessed
              with 'tell'
              
    State   - Control.Monad.State provides State a s which wraps
              s -> (a,s).  State produces a result 'a' but carries
              along with it a 'state' 's' which can be modified
              along the way. Provides 'get, gets, put and modify'
              methods for interacting with 's' (the state)
              
    Cont    - Control.Monad.Cont is used to suspend and resume
              computations as well as transfer control between
              co-routines or other control structures
                  
    Bind (>>=)
    ----------
    
    Allows us to combine two computations into one larger computation.
    i.e. x >>= k is a computation that runs x and uses the output of
         x to decide which computation to run second with the output
         of the second computation being the result of the entire
         computation
         
    Utility Functions in Control.Monad
    ----------------------------------
    
    liftM :: Monad m => (a -> b) -> m a -> m b
        same as fmap, [lifts a normal function into another context]
    
    ap :: Monad m => m (a -> b) -> m a -> m b
        same as (<*>) [sequential application]
        any Monad can be made into an Applicative by setting 
        pure = return and <*> = ap
        
    sequence :: Monad m => [m a] -> m [a]
        takes a list of computations which combines them into one
        computation which collects a list of their individual
        results
        
    replicateM :: Monad m => Int -> m a -> m [a]
        a combination of sequence and replicate
        
    when :: Monad m => Bool -> m () -> m () 
        conditionally executes a computation
        if the test is True, evaluates to the value of the second arg
        otherwise evaluates to 'return ()'
        the 'IfElse' package contains other monadic conditionals
        
    mapM :: Monad m => (a -> m b) -> [a] -> m [b] 
        maps its first argument over the second and sequences
        the results
        
        forM is the mapM function with its arguments reversed
        it models a 'for' loop with [a] acting as loop indices
        and (a -> m b) as the loop body
        
    (=<<) :: Monad m => (a -> m b) -> m a -> m b
        the same as (>>=) with its arguments reversed
        
    (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
        similar to function composition with the arguments
        swapped and an extra 'm' on the result
        there is a reverse variant (<=<)
        
    Other useful functions are: filterM, zipWithM, foldM and
    forever.
        
    Many of the functions have underscore companions i.e.
    sequence_, mapM_, etc. which discard the results of any
    computations performed by their arguments; they are used
    only for their side effects
    
    Laws
    ----
    
    return a >>= k  =  k a    
        if we inject a value 'a' into a monadic context with 'return'
        and then bind to k it is the same as applying k to a
        
    m >>= return    =  m
        binding a monad to return has no effect
    
    m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
        (>>=) is associative, sort of
     
    fmap f xs  =  xs >>= return . f  =  liftM f xs
        ensures fmap and liftM are the same for every instance
        that is both a Functor and Monad (which should be
        EVERY instance of Monad)
        
    The above is the most common representation of the Monad laws
    but another way is think of them in terms of (>=>) ["fish"] which
    composes two functions of type a -> m b and b -> m c
    
    a -> m b is a function from a to b which may have some effect
             in the corresponding context, m
             
    (>=>) lets us compose these 'effectful functions' and lets us
    reformulate the Monad laws as:
    
        return >=> g    = g
        g >=> return    = g
        (g >=> h) >=> k = g >=> (h >=> k)
        
    i.e. 'return' is the 'identity' function and (>=>) is associative.
    
    Do Notation
    -----------
    Do notation is syntactic sugar that roughly equates to bind
    sequences as follows:
    
                          do e → e
               do { e; stmts } → e >> do { stmts }
          do { v <- e; stmts } → e >>= \v -> do { stmts }
        do { let decls; stmts} → let decls in do { stmts } 
        
    i.e.  
        do x <- a       -- result of a bound to x
                b       -- x available to b
           y <- c       -- result of c bound to y
                d       -- x and y available to d
        
        is equivalent to
    
            a >>= \x -> b >> c >>= \y -> d
            
        Note that the value x <- m is entirely dependent on the
        definition of (>>=)

    where 'v' can stand for a pattern as well as a variable
    
    ie  do (x:xs) <- foo
           bar x
           
    And if 'foo' above produces an empty list? Well, that's where
    'fail' kicks in.

--}