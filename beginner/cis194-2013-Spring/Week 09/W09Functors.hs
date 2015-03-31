{-
    Week 09 Functors
        Notes - no Homework exercises for this week
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/
        spring13/lectures/09-functors.html
-}
{-
    Kinds
    -----
        Just as every expression has a type, every type has a
        'kind'. ghci has a :k command to give us kind info
        
        Examples:
        
            Prelude> :k Bool
            Bool :: *
            Prelude> :k Char
            Char :: *
            Prelude> :k Maybe Int
            Maybe Int :: *
            Prelude> :k Maybe
            Maybe :: * -> *
            Prelude>         

        Every type which can act as a value has kind *
        Maybe is a function on types: * -> *; it takes a value
        of some type and transforms it into a value of another
        type.
        
        Many constructors are of the same 'kind' as Maybe
        
            Prelude> :k []
            [] :: * -> *
            Prelude> :k Tree
            Tree :: * -> *
            Prelude> :k (->)
            (->) :: * -> * -> *

            Prelude> :k JoinList            -- from HW07
            JoinList :: * -> * -> *
            
        GHC does 'kind inference' just as it does type inference.
        
            data Funny f a = Funny a (f a)

            Prelude> :k Funny
            Funny :: (* -> *) -> * -> *
                        
        Kind can also be partially applied:
        
            Prelude> :k Funny Maybe
            Funny Maybe :: * -> *

            Prelude> :k Funny Maybe Int
            Funny Maybe Int :: *            
        
-}
{-
    Functor (the name comes from Category Theory)
    -------
        - generalizes the common pattern of applying a function
          to every element in a container; the essential pattern is:
          
                thingMap :: (a -> b) -> f a -> f b
        
        where 'f' is a type variable standing for some 'kind' of
        thing (* -> *)
        
        thingMap has to work differently for each type of thing
        (each container) i.e. we need different fmap implementations
        for each container type: List, Maybe, Either, IO, etc
        
            instance Functor IO where
                fmap f ioa = ioa >>= (return . f)
       
        A more complex example is:
        
            instance Functor ((->) e) where
              fmap = (.)        
              
        If we think about it, f = (->) e so
            fmap :: (a -> b) -> (->) e a -> (->) e b
            
        or, using (->) as an infix operator
            fmap :: (a -> b) -> (e -> a) -> (e -> b)
            
        We can think of the type (e -> a) as an 'e-indexed container'
        with one value, 'a', for each 'e'. Mapping a function over
        each value corresponds to function composition:
            1. apply (e -> a) to the container to pick the first value
            2. apply (a -> b) to the chosen element
            3. repeat until the container is empty
-}