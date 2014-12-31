{-
    Problem 1
    
    Find the last element of a list.

    (Note that the Lisp transcription of this problem is incorrect.)

    Example in Haskell:

    Prelude> myLast [1,2,3,4]
    4
    Prelude> myLast ['x','y','z']
    'z'
-}

myLast :: [a] -> a
myLast []     = error("empty list")
myLast [x]    = x
myLast (x:xs) = myLast xs
       
-- all of the following alternatives produce an error
-- if given an empty list
--       
-- using existing library functions other than 'last'
myLast_a    = head . reverse
myLast_b xs = head (drop (length xs - 1) xs)
myLast_c xs = xs !! (length xs - 1)

-- alternative solutions from the H99 site
myLast_d = foldr1 (const id)
myLast_e = foldr1 (flip const)  -- treats entire list as a constant
myLast_f = foldl1 (curry snd)

-- tests
testIntList  fn = fn [1,2,3,4]     == 4
testCharList fn = fn ['x','y','z'] == 'z'

tests =  testIntList myLast     && testCharList myLast
      && testIntList myLast_a   && testCharList myLast_a
      && testIntList myLast_b   && testCharList myLast_b
      && testIntList myLast_c   && testCharList myLast_c
      && testIntList myLast_d   && testCharList myLast_d
      && testIntList myLast_e   && testCharList myLast_e
      && testIntList myLast_f   && testCharList myLast_f