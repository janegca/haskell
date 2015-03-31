{-
    Week 10 Applicative Functors - Part 1
        Notes from reading
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        10-applicative.html
        
    Reading: Typeclassopedia  4: Applicative
        https://wiki.haskell.org/Typeclassopedia#Applicative
-}
{-
    A functor is allows us to apply a function to the values of
    a container i.e. it 'lifts' a normal function into the containers
    world (the container becomes the 'context' in which the normal
    function is applied).  However, if we can't use that lifted
    function (functor) in another context. The Applicative class 
    allows us to do just that; it lifts a functor into another
    context.
    
        class Functor f => Applicative f where
        pure  :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b

    where <*> is 'functor application' or 'function application
              in a computational context'
              
          f (a -> b) is a functor (a function in a context)
          
          pure - takes any type and returns it in a context/container
                 of type a
                 
    Laws
    ----
        Identity        pure id <*> v = v
        
        Homomorphism    pure f <*> pure x =  pure (f x)
                            allows collapsing multiple adjacent pure's
                            into one
                            
        Interchange     u <*> pure y = pure ($ y) <*> u
                            allows moving of pure leftward
                            
        Composition     u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
                            allows reassociating <*>
        
        Functor         fmap g x = pure g <*> x
                        g <$> x  = pure g <*> x
                            mapping a pure function g over a context x
                            is the same as injecting g into a context
                            with pure and then applying it to x
                            
                            <$> is defined as a synonym for fmap
                            
    Instances
    ---------
        There are two different applicative instances defined over
        lists. The first, ZipList, takes a list of functors and
        applies them, position wise, to a list [deterministic??]
        
        newtype ZipList a = ZipList { getZipList :: [a] }
         
        instance Applicative ZipList where
          pure x = ZipList (repeat x)  -- infinite list
          (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
          
        The second is based on non-deterministic computation; each
        functor is applied to every element in the list
        
        instance Applicative [] where
          pure x    = [x]
          gs <*> xs = [ g x | g <- gs, x <- xs ]
          
        This allows us to write:
            (+) <$> [2,3,4] <*> pure 4
            
        which returns [6,7,8]
        
            (+) <$> [2,3,4] <*> pure 4
         -> fmap (+) [2,3,4] <*> [4]
         -> [(2+),(3+),(4+)] <*> [4]
         -> [(2+4),(3+4),(4+4)]
         -> [6,7,8]
        
    [Notes:
        A Functor is a data structure that comes with a function, fmap,
        which lets us convert a regular function into a function that works
        over elements in this particular data structure.
        i.e. fmap, when given a normal function, will apply that function
             to every element in the data structure; it 'lifts' the
             normal function into the world (context) of the data structure
             
        An Applicative Functor takes a data structure containing
        partially applied functors and allows them to be applied
        to every element in a second data structure of the same type.
        ie <*> lifts a functor into the world (context) of a second
           world having the same data structure [not sure on this
           yet, can you take a list of functors and apply them over
           a tree?]
    ]          
    
    IO Instance
    -----------
        m1 <*> m2
        m1 is executed, yielding the function f, then m2 is executed
        yeilding a value x and the final result (f x) is returned
    
    Applicative Style
    -----------------
        The idealized notation is:
            [[g x1 x2 x3 ... xn]]
            
        Haskell notation is:
    
        g <$> x1 <*> x2 <*> x3 <*> ... <*> xn
        
        denotes the mapping of the functor g over all elements x1..xn
        
        And 'pure' allows us to embed elements that we don't want
        touched
        
        g <$> x1 <*> pure x2 <*> x3 <*> ... <*> xn
        
        ie the application of g to (pure x2) will return x2
        
    Other Applicative Operators
    ---------------------------
        (<*)    sequential, discard second argument
        (>*)    sequential, discard first argument
        (<**>)  <*> with arguments reversed (flip for <*>)
        (<$)    replace all locations in the input with the same
                value (fmap . const)
        
-}