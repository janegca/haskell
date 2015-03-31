{-
    Week 09 Functors
        Notes from recommended readings
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/
        spring13/lectures/09-functors.html
        
    Reading: Learn You a Haskell - The Functor typeclass
        http://learnyouahaskell.com/making-our-own-types-and-typeclasses
            #the-functor-typeclass
-}
{-
    The Functor typelass
    --------------------
        - basically for things that can be mapped over
        
            class Functor f where  
                fmap :: (a -> b) -> f a -> f b 
                
            Note that 'f' here stands for a type constructor
            so 'f a' is a type constructor that takes one parameter
            which can be of any type ie Maybe a
            
        - a list is an instance of Functor
        
            instance Functor [] where  
                fmap = map  
                
            Note that [] is a type constructor; [a] would be
            a list of type 'a'; we want Functor to apply to
            any type of list, not a specific type.
            
        - another instance is Maybe

            instance Functor Maybe where  
                fmap f (Just x) = Just (f x)  
                fmap f Nothing = Nothing  
                
        - can write implementatoins for custom types

-}
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

instance Functor Tree where  
    fmap f EmptyTree      = EmptyTree  
    fmap f (Node x t1 t2) = Node (f x) (fmap f t1) (fmap f t2)         

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)          
        
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right          
    
ex1 = fmap (*2) EmptyTree
ex2 = fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
    