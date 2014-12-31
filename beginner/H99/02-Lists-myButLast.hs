{-
    Problem 2
    
    (*) Find the last but one element of a list.

    (Note that the Lisp transcription of this problem is incorrect.)

    Example in Haskell:

    Prelude> myButLast [1,2,3,4]
    3
    Prelude> myButLast ['a'..'z']
    'y'

-}
import qualified Data.Foldable as F

myButLast :: [a] -> a
myButLast []       = error("Empty List")
myButLast (x:y:[]) = x
myButLast (_:xs)   = myButLast xs

-- using existing library functions other than 'last'
myButLast_a xs = head . reverse $ init xs
myButLast_b xs = head $ drop (length xs - 2) xs
myButLast_c xs = xs !! (length xs - 2)
myButLast_d    = foldr1 (const id) . init
myButLast_e    = foldr1 (flip const) . init
myButLast_f    = foldl1 (curry snd) . init

-- alternative solutions from the H99 site
myButLast_g xs = reverse xs !! 1
myButLast_h    = last . init
myButLast_i    = head . tail . reverse

myButLast_j :: F.Foldable f => f a -> a
myButLast_j = fst . F.foldl (\(a,b) x -> (b,x)) (err1,err2)
  where
    err1 = error "myButLast_j: Empty list"
    err2 = error "myButLast_j: Singleton"
    
-- tests
testIntList  fn = fn [1,2,3,4]     == 3
testCharList fn = fn ['x','y','z'] == 'y'

tests =  testIntList myButLast     && testCharList myButLast
      && testIntList myButLast_a   && testCharList myButLast_a
      && testIntList myButLast_b   && testCharList myButLast_b
      && testIntList myButLast_c   && testCharList myButLast_c
      && testIntList myButLast_d   && testCharList myButLast_d
      && testIntList myButLast_e   && testCharList myButLast_e
      && testIntList myButLast_f   && testCharList myButLast_f
      && testIntList myButLast_g   && testCharList myButLast_f
      && testIntList myButLast_h   && testCharList myButLast_h
      && testIntList myButLast_i   && testCharList myButLast_i
      && testIntList myButLast_j   && testCharList myButLast_j
      
      
      