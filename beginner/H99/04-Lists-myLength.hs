{-
    Problem 4

    (*) Find the number of elements of a list.

    Example in Haskell:

    Prelude> myLength [123, 456, 789]
    3
    Prelude> myLength "Hello, world!"
    13    

-}

myLength :: [a] -> Int
myLength xs = sum [ 1 | _ <- xs]

-- alternative methods
myLength_a :: [a] -> Int
myLength_a []     = 0
myLength_a (x:xs) = 1 + myLength_a xs

myLength_b = sum . map (\_ -> 1)

myLength_c = foldr1 (+) . map (\_ -> 1)

-- other methods from H99 site
myLength_d =  foldl (\n _ -> n + 1) 0
myLength_e =  foldr (\_ n -> n + 1) 0

myLength_f =  foldr (\_ -> (+1)) 0
myLength_g =  foldr ((+) . (const 1)) 0

myLength_h =  foldr (const (+1)) 0
myLength_i =  foldl (const . (+1)) 0

-- zip elements with index numbers and extract last
myLength_j xs = snd $ last $ zip xs [1..] 
myLength_k    = snd . last . (flip zip [1..]) 
myLength_l    = fst . last . zip [1..]

-- tests
testIntList  fn = fn [123, 456, 789] == 3
testCharList fn = fn "Hello, world!" == 13

tests =  testIntList myLength     && testCharList myLength
      && testIntList myLength_a   && testCharList myLength_a
      && testIntList myLength_b   && testCharList myLength_b
      && testIntList myLength_c   && testCharList myLength_c
      && testIntList myLength_d   && testCharList myLength_d
      && testIntList myLength_e   && testCharList myLength_e
      && testIntList myLength_f   && testCharList myLength_f
      && testIntList myLength_g   && testCharList myLength_g
      && testIntList myLength_h   && testCharList myLength_h
      && testIntList myLength_i   && testCharList myLength_i
      && testIntList myLength_j   && testCharList myLength_j
      && testIntList myLength_k   && testCharList myLength_k
      && testIntList myLength_l   && testCharList myLength_l
      
       