{-
    Week 10 Applicative Functors - Part 1
        Notes from reading
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        10-applicative.html
        
    Reading: Learn You a Haskell - Applicative Functors
        http://learnyouahaskell.com/
            functors-applicative-functors-and-monoids#applicative-functors
        
-}
{-
    Applicative Functors
    --------------------
        - 'beefed up' functors
        - represented by the Applicative class found in 
          Control.Applicative
          
        What happens when we map a function that takes more than
        one parameter over a functor? For example, what is the type
        of:
                fmap (*) (Just 3)
                
            Prelude> :t fmap (*) (Just 3)
            fmap (*) (Just 3) :: Num a => Maybe (a -> a)
            Prelude> 

        We get a function wrapped in a Maybe: Just (* 3)
        i.e. because of currying we get a partially applied function
        
        If we map compare over a list of characters we get:
        
            Prelude> :t fmap compare "A List of Chars"
            fmap compare "A List of Chars" :: [Char -> Ordering]
            Prelude>        
            
        i.e. a list of functions of the type: Char -> Ordering
        because 'compare' has the type: Ord a => a -> a -> Ordering
        and, in this instance, 'a' is the Char type
        
        The net result is that, if we map multi-parameter functions
        over functors we get back functors that contain functions.
        We can, in turn, map these 'functor functions' over
        containers
-}
import Control.Applicative

ex1 = fmap (*) [1,2,3,4]        -- [(* 1), (* 2), (* 3), (* 4)]
ex1a = fmap (\f -> f 9) ex1     -- [9,18,27,36]

-- in ex1a, '9' is supplied as the second argument to each of the
-- sections so (* 1) 9 = (1 * 9) = 9, (* 2) 9 = (2 * 9) = 18, etc.

{-
    What happens if we have functor values and we want to map
    them over other functor values? 
    i.e. what if we had Just (* 3) and we wanted to map it over
         Just 5?
         
    We can't do it with normal functors as they only map normal
    functions over functors.  That's where the Applicative type
    class comes in.
    
        class (Functor f) => Applicative f where  
            pure  :: a -> f a  
            (<*>) :: f (a -> b) -> f a -> f b  

    First, only members of the Functor class can be made members of
    the Applicative class.
    
    The 'pure' method takes any type 'a' and returns a functor (f)
    that contains the value 'a' i.e. it places the value 'a' in
    a specific 'context'.
    
    (<*>) is a beefed up fmap; it takes a functor that wraps a
    function which it applies to another functor which produces
    a third functor. Left-associative, sequential application.
    
    An example of Maybe as an instance of Applicative
    
        instance Applicative Maybe where  
            pure                   = Just  
            Nothing  <*> _         = Nothing  
            (Just f) <*> something = fmap f something     
-}
-- examples
ex2a = Just (+3) <*> Just 9             -- Just 12
ex2b = pure (+3) <*> Just 10            -- Just 13
ex2c = pure (+3) <*> Just 9             -- Just 12
ex2d = Just (++ "hahah") <*> Nothing    -- Nothing
ex2e = Nothing <*> Just "woot"          -- Nothing

-- pure f <*> x equals fmap f x
-- pure puts a value in a default context
ex3a = pure (+ 3) <*> Just 5            -- Just 8
ex3b = fmap (+ 3) (Just 5)              -- Just 8

-- Control.Applicative exports <$> which is the infix fmap operator
ex4a = (+ 3) <$> (Just 5)               -- Just 8

-- an example of using a normal function on applicative functors
ex4b = (++) <$> Just "johntra" <*> Just "volta" -- Just "johntravolta"

{-
    Walkthrough:
    
            (++) <$> Just "johntra" <*> Just "volta"
         -> fmap  (++) "johntra" <*> Just "volta"
         -> Just ("johntra" ++) <*> Just "volta"
         -> Just ("johntra" ++) "volta"
         -> Just "johntra" ++ "volta"
         -> Just "johntravolta"

-}
{-
    List as an instance of Applicative:
    
        instance Applicative [] where  
            pure x    = [x]  
            fs <*> xs = [f x | f <- fs, x <- xs]     

-}
ex5a = [(*0),(+100),(^2)] <*> [1,2,3]  -- [0,0,0,101,102,103,1,4,9]
ex5b = [(+),(*)] <*> [1,2] <*> [3,4]   -- [4,5,5,6,3,4,6,8]

{-
    Walkthrough (remember that <*> is left-associative):
    
        [(+),(*)] <*> [1,2] <*> [3,4]
     -> ([(+),(*)] <*> [1,2]) <*> [3,4]
     -> [(1+),(2+),(1*),(2*)] <*> [3,4]
     -> [ (1+3), (1+4), (2+3), (2+4), (1*3), (1*4), (2*3), (2*4)]
     -> [ 4, 5, 5, 6, 3, 4, 6, 8]
-}
ex5c = (++) <$> ["ha","heh","hmm"] <*> ["?","!","."] 
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

{-
    IO is also Applicative
    
        instance Applicative IO where  
            pure    = return  
            a <*> b = do  
                f <- a  
                x <- b  
                return (f x)     

-}
-- prompt the user for two strings, concatenate the two and return
-- one string as the result
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b  

-- the same written in applicative style
myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

{-
        *Main> myAction
        hello
        world
        "helloworld"
        *Main> 
        *Main> myAction'
        hello
        world
        "helloworld"
        *Main> 
-}    
-- another version combining both techniques
ex7 = do  
        a <- (++) <$> getLine <*> getLine  
        putStrLn $ "The two lines concatenated turn out to be: " ++ a
        
{-
    *Main> ex7
    hello
    world
    The two lines concatenated turn out to be: helloworld
    *Main> 
-}        
-- calling <*> with two applicative functors
ex8 = (+) <$> (+3) <*> (*100) $ 5       -- 508

{-
        (+) <$> (+3) <*> (*100) $ 5
     -> fmap (+) (+ 3 5) (* 100 5)
     -> fmap (+) 8 500
     -> 8 + 500
     -> 508     
-}
-- we can think of functions as containers which hold their
-- eventual results i.e. k <$> f <*> g

{-
    Applicative ZipList applies functions based on their
    positions i.e. first element of list 1 applied to first element of
    list 2, second elment of list 1 applied to second element
    of list 2, etc.
    
        instance Applicative ZipList where  
                pure x                    = ZipList (repeat x)  
                ZipList fs <*> ZipList xs = 
                    ZipList (zipWith (\f x -> f x) fs xs)      

-}
ex9a = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
ex9b = getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
ex9c = getZipList $ (,,) <$> ZipList "dog" 
                         <*> ZipList "cat" <*> ZipList "rat"  

{-

    *Main> ex9a
    [101,102,103]
    *Main> ex9b
    [5,3,3,4]
    *Main> ex9c
    [('d','c','r'),('o','a','a'),('g','t','t')]
    *Main> 

-}
{-
    liftA2 takes a binary function and promotes it to a function
    that works on two functors

-}
ex10a = liftA2 (:) (Just 3) (Just [4])
ex10b = (:) <$> Just 3 <*> Just [4]

-- a function that takes a list of applicatives and returns an
-- applicative that has a list as its result value
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA []     = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- another way to define the function
sequenceA' :: (Applicative f) => [f a] -> f [a]  
sequenceA' = foldr (liftA2 (:)) (pure [])

ex11a = sequenceA [Just 3, Just 2, Just 1]      -- Just [3,2,1]
ex11b = sequenceA [Just 3, Nothing, Just 1]     -- Nothing
ex11c = sequenceA [(+3),(+2),(+1)] 3            -- [6,5,4]
ex11d = sequenceA [[1,2,3],[4,5,6]]
    -- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
    
ex11e = sequenceA [(>4),(<10),odd] 7            -- [True, True, True]
ex11f = sequenceA [[1,2],[3,4],[5,6]] 
   -- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
   
ex11fa = [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]
   -- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]] 
   
{-
    
    Walkthrough 
        <$> and <*> are both infixl 4, (:) is infixr 5
        the type of fmap (:) [1,2] is Num a => [[a] -> [a]]
        so, a function that takes and returns a list of lists

        sequenceA [[1,2],[3,4],[5,6]] 
     -> sequenceA ([1,2]:xs) = (:) <$> [1,2] <*> sequenceA xs
     -> fmap (:) [1,2] <*> sequenceA xs
     -> ([(1:),(2:)] <*> (:) <$> [3,4])   <*> sequenceA xs
     -> ([(1:),(2:)] <*> fmap (:) [3,4])  <*> sequenceA xs
     -> ([(1:),(2:)] <*> [(3:),(4:)])     <*> sequenceA xs
     -> ([(1:3:), (1:4:), (2:3:),(2:4:)]) <*> (:) <$> [5,6] <*> sequenceA []
     -> ([(1:3:), (1:4:), (2:3:),(2:4:)]) <*> fmap (:) [5,6] <*> sequenceA []
     -> ([(1:3:), (1:4:), (2:3:),(2:4:)]) <*> [(5:),(6:)] <*> sequenceA []
     -> [(1:3:5), (1:3:6), (1:4:5), (1:4:6), 
         (2:3:5), (2:3:6), (2:4:5), (2:4:6)] <*> sequenceA pure []
     -> [ (1:3:5:[]), (1:3:6:[]), (1:4:5:[]), (1:4:6:[]),
          (2:3:5:[]), (2:3:6:[]), (2:4:5:[]), (2:4:6:[]) ]
     -> [ [1,3,5], [1,3,6], [1,4,5], [1,4,6],
          [2,3,5], [2,3,6], [2,4,5], [2,4,6] ]
                
-}   
{-
    Laws
    ----
        pure f <*> x = fmap f x
        pure id <*> v = v
        pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
        pure f <*> pure x = pure (f x)
        u <*> pure y = pure ($ y) <*> u

-}
    

