{-
    Week 12 Monads
        Notes and exercises
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/12-monads.html
-}
{-
    The main difficulty with using the Applicative style is that
    it gives no way to make a decision based on previous results.
    A Monad does not suffer from this limit.
    
    The key method in the Monad class is 'bind' (>>=)
    
        (>>=) :: m a -> (a -> m b) -> m b
        
        bind takes two arguments, a value of type 'm a' which represents
        a computation which results in one or more values of type 'a'
        and it may also of some sort of 'effect'
        
    eg  c1 :: Maybe a
            is a computation that might fail or which might produce
            a result in 'a'
            
        c2 :: [a] is a computation that will result in multiple a's
        
        c3 :: Parser a
            a computation that implicitly consumes a String and
            possibly produces 'a'
            
        c4 :: IO a
            a computation that potentially has some IO effects
            and produces an 'a'
            
    [Note: remember that 'a' is a type variable so what is being
           produced is a value of some type 'a']
           
    The second argument is a function: (a -> m b)
        it will choose what computation to run based on the result
        of the first computation
        
        i.e. it "embodies the promised power of Monad to encapsulate 
              computations which can choose what to do next based on the 
              results of previous computations."
              
    What (=>) does is combine two computations to produce one larger
    computation
-}
check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7  >>= check >>= halve        -- Nothing
ex02 = return 12 >>= check >>= halve        -- Nothing
ex03 = return 12 >>= halve >>= check        -- Just 6

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex04 = [10,20,30] >>= addOneOrTwo           -- [11,12,21,22,31,32]

{-
    Monad Combinators
    -----------------
    Using 'return' and (>>=) we can build a number of general
    combinators like 'sequence' and 'replicateM'
    
        sequence :: Monad m => [m a] -> m [a]
        sequence [] = return []
        sequence (ma:mas) =
          ma >>= \a ->
          sequence mas >>= \as ->
          return (a:as)    
          
        replicateM :: Monad m => Int -> m a -> m [a]
        replicateM n m = sequence (replicate n m)       

    We could thus have written homework functions for Parser.hs
    as:

        parseFile :: Parser [[Int]]
        parseFile = many parseLine

        parseLine :: Parser [Int]
        parseLine = parseInt >>= \i -> replicateM i parseInt
        
    to allow us read files with structures like:
    
        4 78 19 3 44 3 1 7 5 2 3 2
        
    where the first number tells how many numbers are in the first
    group; the next, how many numbers in the second group, etc so
    
        incl #'s    group
            4       79 19 3 44      - first group
            3       1 7 5           - second group
            2       3 2             - third group

-}
