module L03B where

-- code from the course .pdf's

{- 
    Difference between 'instructions' and 'functions' is that 
    a function (in a mathematical sense) will ALWAYS return the same
    output given the same input while 'instructions' can behave
    differently on different occasions [can produce different outputs
    for the same inputs, and may produce side-effects]
    
    Following is from Thompson text, Chapter 18
    
    "Monads...make explicit the sequence in which operations take
     place." 
     
    "...monads provide an important structuring mechanism for
     program construction, as they encourage 'separation of concersn'"
     
    The 'do' construct is based on the combinator (>>=)
        (>>=) :: m a -> (a -> m b) -> m b
        
    Monad class:
    
        class Monad (m :: * -> *) where
            (>>=)  :: m a -> (a -> m b) -> m b
            return :: a -> m a
            (>>)   :: m a -> m b -> m b
            fail   :: String -> m a   

    'Monad' is a 'constructor' class; it has 'type constructors' 
    i.e. it builds 'types' from other 'types'. Instances of Monad
    are:
        instance Monad (Either e) -- Defined in ‘Data.Either’
        instance Monad Maybe      -- Defined in ‘Data.Maybe’
        instance Monad []         -- Defined in ‘GHC.Base’
        instance Monad IO         -- Defined in ‘GHC.Base’
        instance Monad ((->) r)    -- Defined in ‘GHC.Base’
        
    (>>) and fail have default implementations:
        m >> k = m >>= \_ -> k    -- like (>>=) but value from the
                                  -- the first arg is discarded
        fail s = error s
        
    A 'monad' is a generalization of 'function composition'; it
    'composes' objects
        f >@> g = \x -> (f x) >>= g
        
        where >@> is a 'derived' operator
    
    We can think of monad 'm a' as representing a computation with
    elements of 'm a' being computations that perform actions before
    returning a value of type a.
    
    Example of some standard functions over monads
        
        mapF :: Monad m => (a -> b) -> m a -> m b
        mapF f m = do x <- m
                      return (f x)
                      
        joinM :: Monad m => (a -> b) -> m a -> m b     
        joinM m = do x <- m
                     x
                     
    Over lists, the above equate to map and concat
-}

-- create a file named 'foo' and write the word 'baz' to the file
ex1 = writeFile "foo" "baz"

-- 'io' represents 'instructions'
display io =
    do result <- io     -- follow instructions to get a value
       print result     -- print that value
       
ex2 = display (readFile "foo")    -- displays the contents of 'foo'

{-
    do notation (from Thompson)
        used to sequence I/O programs
        used to 'capture' values from IO actions and pass them on
        to the actions that follow 
-}

{-
    ex2 Output:
        *L03B> ex2
        "baz"
-}
       
doTwice :: Monad a => a b -> a (b,b)    
doTwice io =
    do a <- io
       b <- io
       return (a,b)
       
ex3 = display (doTwice (print "hello"))       
{-
    Output:
    
        *L03B> ex3
        "hello"
        "hello"
        ((),())
    
-}       
       
-- ignore any IO instructions    
dont :: Monad a => a b -> a ()   
dont io = return ()   
ex4 = display (dont (print "hello"))
{-
    Output:
    
        *L03B> ex4
        ()
    
-}    

printTable :: [String] -> IO ()
printTable xs = sequence_ [print (show i ++ ": " ++ x) 
                 | (x,i) <- zip xs [1..]]
       
ex5 = printTable ["apa","bepa","cepa"]       
       
-- Examples from Chapter 18 of Thompson text
putNtimes :: Int -> String -> IO ()
putNtimes n str =
    if n <= 1
    then putStrLn str
    else do putStrLn str
            putNtimes (n-1) str

ex6 = putNtimes 4 "hello world"

getNput :: IO ()
getNput = do 
            putStrLn "Enter some text: "
            line <- getLine
            putStrLn line

-- modifying input            
reverse2lines :: IO ()
reverse2lines =
    do  linel <- getLine
        line2 <- getLine
        let revl = reverse linel
        let rev2 = reverse line2
        putStrLn rev2
        putStrLn revl            

-- use 'read' to convert a String to a specific type        
getInt :: IO Int
getInt = do line <- getLine
            return (read line :: Int)        

-- use recursion to implement a loop structure
goUntilEmpty :: IO ()
goUntilEmpty =
    do  line <- getLine    -- every '<-' creates a NEW variable
        if (line == [])
        then return ()
        else (do putStrLn line
                 goUntilEmpty)            
            
-- small interactive program to sum integers
sumInteract :: IO ()
sumInteract =
    do  putStrLn "Enter integers one per line"
        putStrLn "These will be summed until zero is entered"
        sum <- sumInts
        putStr "The sum was "
        print sum
    where
        sumInts :: IO Int
        sumInts = 
            do  n <- getInt
                if n == 0
                then return 0
                else (do m <- sumInts
                         return (n + m) )            
                         
-- the 'do' construct is based on the sequence 'then' operator (>>=)
addOneInt :: IO ()
addOneInt =
    do  line <- getLine
        putStrLn (show (1 + read line :: Int))               

-- is syntactic sugar for
addOneInt' :: IO ()
addOneInt' = getLine >>= \line ->
             putStrLn (show (1 + read line :: Int))        
             
-- structuring a Tree as a monad
data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving Show

-- sum the values in a tree of Ints, recursive approach
sTree :: Tree Int -> Int
sTree Nil            = 0
sTree (Node n t1 t2) = n + sTree t1 + sTree t2

-- monadic approach
data Id a = Id a
    
instance Monad Id where
    return         = Id
    (>>=) (Id x) f = f x
    
-- take the wrapper of Id     
extract :: Id a -> a
extract (Id x) = x    

sumTree :: Tree Int -> Id Int
sumTree Nil = return 0
sumTree (Node n t1 t2) =
    do  num <- return n
        s1  <- sumTree t1
        s2  <- sumTree t2
        return (num + s1 + s2)
     
-- example of using a State monad
-- we want to replace every element in a Tree with a number
-- elements with the same values are to be replaced with the same
-- number

type Table a   = [a]
data State a b = State (Table a -> (Table a,b))

instance Monad (State a) where
    -- leave the state unchanged when returning a value
    return x = State (\tab -> (tab,x))
    
    -- create a new state and pass it to the function so
    -- operations are performed using the new state
    (State st) >>= f =
        State (\tab -> let (newTab, y)   = st tab
                           (State trans) = f y
                        in  trans newTab)

extract' :: State a b -> b
extract' (State st) = snd (st [])                        
                                               
numberTree :: Eq a => Tree a -> State a (Tree Int)
numberTree Nil            = return Nil
numberTree (Node x t1 t2) =
    do num <- numberNode x
       nt1 <- numberTree t1
       nt2 <- numberTree t2
       return (Node num nt1 nt2)
       
numberNode :: Eq a => a -> State a Int
numberNode x = State (nNode x)

nNode :: Eq a => a -> (Table a -> (Table a , Int ) )
nNode x table
    | elem x table = (table,lookup' x table)
    | otherwise    = (table ++ [x], length table)

-- returns the index of the elem in the table
lookup' :: Eq a => a -> Table a -> Int
lookup' x tab = locate 0 tab
    where
        locate n (y:ys) =  if x == y 
                           then n 
                           else locate (n+1) ys

numTree :: Eq a => Tree a -> Tree Int
numTree = extract' . numberTree        

exTree :: Tree String
exTree = Node "Moon"
            (Node "Ahmet" Nil Nil)
            (Node "Dweezil"
                (Node "Ahmet" Nil Nil)
                (Node "Moon"  Nil Nil))

ex7 = numTree exTree                

{-
    Output:
    
        *L03B> ex7
        Node 0                          -- "Moon"
          (Node 1 Nil Nil)              -- "Ahmet"
          (Node 2 
            (Node 1 Nil Nil)            -- "Ahmet"
            (Node 0 Nil Nil))           -- "Moon"
    
-}
             