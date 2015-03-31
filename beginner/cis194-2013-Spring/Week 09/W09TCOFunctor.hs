{-
    Week 09 Functors
        Notes from recommended readings
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/
        spring13/lectures/09-functors.html
        
    Reading: Typeclassopedia - 3. Functor
        https://wiki.haskell.org/Typeclassopedia#Functor
-}
{-
    Functor
    -------
        - a functor represents a  container and the ability to
          apply a function to every element in that container
        - a functor represents a "contextual concept"
        
    Functor Definition
    ------------------
        Part of the Prelude
        
        class Functor f where
            fmap :: (a -> b)            -- any function from a to b
                 -> f a                 -- a value of type f a
                 -> f b                 -- a value of type f b
            
        where 'f' is a 'type constructor' that takes one parameter
        i.e. the 'kind' of 'f' is * -> *
        
        fmap applies the function to each element of a container
        without changing the structure of the container
        i.e. fmap applies the function to a value without altering
             its context (its container)
             
    Functor Instances
    -----------------
    
        instance Functor [] where
          fmap _ []     = []
          fmap g (x:xs) = g x : fmap g xs
          -- or we could just say fmap = map
         
        instance Functor Maybe where
          fmap _ Nothing  = Nothing
          fmap g (Just a) = Just (g a)   

    - there are many others, some need to be imported from 
      Control.Monad.Instances
    
    - many standard container types (Tree, Map, Sequence) are
      also instances of Functor
      
    - others, such as Set, are not; Set has an Ord constraint,
      fmap must apply to any a and b
      
    - you can make a Set part of Functor by adding a constraint
    
    Laws
    ----
        A well defined Functor will satisfy the following:
        
            fmap id      = id
            fmap (g . h) = (fmap g) . (fmap h)
            
        These laws ensure the structure of the container is
        not changed.
        
        GHC can automatically derive Functor instances for many
        data types.
        
    Intuition : Lift
    ----------------
        fmap, like all Haskell functions, is curried; it takes
        a normal function (g :: a -> b) and transforms it into
        a function (fmap g :: f a -> f b) that operates over
        containers/contexts. This transformation is called a
        'lift' ... fmap 'lifts' a function from the 'normal world'
        into the 'f world' (functor world)
-}
