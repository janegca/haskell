-- partial application of functions
{-
    Note:
        all functions in Haskell are 'one argument' functions
        hence the arrows in the type signature
        When 'add 1 2' is executed, Haskell creates a function
        'add 1' and then applies it to the 2nd argument, 2,
        the result is the 'value' of ((add 1)2)
        
        A function that took 3 arguments would have the 'value'
            (((f a) b) c)
            
        i.e. a 'function' is simply a way to represent simple
             or complex values; in a sense, an entire application
             is also the representation of a 'value'; the result
             of which is entirely dependent on the 'input'
-}

-- binary function (takes two arguments)
add :: Num a => a -> a -> a
add a b = a + b

-- fix 'a' value at 3
add3 :: Integer -> Integer
add3 = add 3

-- successive multiples of a given number, n
multiples_of :: (Num a, Enum a) => a -> [a]
multiples_of n = [n,n*2..]

-- fix 'n' at 43 (Note that this is now simply a 'value',
-- and not a 'function value')
multiples_of_43 :: [Integer]
multiples_of_43 = multiples_of 43

{-
    [1 of 1] Compiling Main             ( 04-currying.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> add 1 2
    3
    *Main> add3 5
    8
    *Main> take 5 (multiples_of 10)
    [10,20,30,40,50]
    *Main> take 5 multiples_of_43
    [43,86,129,172,215]
    *Main> 

-}