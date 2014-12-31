{- Lazy evaluation in Haskell allows
   for cool things, like the definition
   of infinite lists!
-}

{-
    The following code segments construct lists.
    They do so by giving the first term and something from which a pattern 
    can be gathered.

    The '..' means to continue the pattern until the end term is reached.
    If there is no given end term, the list constructed will be an infinite 
    list.
-}

naturals = [1..]

--This constructs an infinite list with the first term of 1,
--the pattern being that the next term will be 1+ the previous list term 
--(the default pattern).

even_numbers = [0,2..]
--This constructs an infinite list with the first two terms of 0 and 2.
--Between these first two terms, a pattern of adding 2 to each successive 
--term is noted.
--The rest of the list is constructed using this pattern, adding 2 to 
--each term to get the next.

even_numbers' = map (*2) naturals
--This function maps the operation (*2) to the list naturals,
--constructing a list where each value is 2 times that of the 
--corresponding value in naturals.

odd_numbers = map (+1) even_numbers
--This function maps the operation (+1) to the list even_numbers,
--constructing a list in which each value is one more than that of the 
--corresponding value in even_numbers.

odd_numbers' = [1,3..]  -- another way to generate odd numbers

{-
    Example output 
      using 'take 10' (to avoid outputting infinite lists)
    
    Prelude> :load "02_infinity.hs"
    [1 of 1] Compiling Main           ( 02_infinity.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> take 10 naturals
    [1,2,3,4,5,6,7,8,9,10]
    *Main> take 10 even_numbers
    [0,2,4,6,8,10,12,14,16,18]
    *Main> take 10 even_numbers'
    [2,4,6,8,10,12,14,16,18,20]
    *Main> take 10 odd_numbers
    [1,3,5,7,9,11,13,15,17,19]
    *Main> take 10 odd_numbers'
    [1,3,5,7,9,11,13,15,17,19]
    *Main> 

-}