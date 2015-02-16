-- Chapter 13 - Circuit Design - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Stdm


{- Exercise 1
        Design a circuit to implement the following truth table
        
                a   b   c  f a b c
                ---------  -------
                0   0   0     1
                0   0   1     1
                0   1   0     0
                0   1   1     0
                1   0   0     0
                1   0   1     1
                1   1   0     1
                1   1   1     1
-}

abc :: [(Bool, Bool, Bool)]
abc = [(False, False, False),
       (False, False, True),
       (False, True,  False),
       (False, True,  True),
       (True,  False, False),
       (True,  False, True),
       (True,  True,  False),
       (True,  True,  True)]
       
f1 :: Bool -> Bool -> Bool -> Bool
f1 a b c  = not a && not b && not c
         || not a && not b && c
         || a     && not b && c
         || a     && b     && not c
         || a     && b     && c
     
f1out :: [Bool]     
f1out = [True, True, False, False, False, True, True, True]         

ex1 :: Bool
ex1 = [ f1 a b c | (a,b,c) <- abc] == f1out

{-
    Exercise 2
    
    Recall the informal description of the multiplexor: if a is 0 then
    the output is x, but if a is 1 then the output is y. 
    Write a truth table that states this formally, and then use the 
    procedure from Section 13.2.2 to design a multiplexor circuit. 
    Compare your solution with the given definition of mux1. 
    
            a   x y
            -------
            0    x
            1    y
-}

-- multiplexer from text
mux1 :: Signal a => a -> a -> a -> a
mux1 a x y = or2 (and2 (inv a) x) (and2 a y)

f2 :: Bool -> a -> a -> a
f2 a x y = if not a then x else y

ex2 = (f2   False True False) == True
   && (f2   True  True False) == False
   && (mux1 False True False) == True
   && (mux1 True  True False) == False
   
{-
    mux1 walkthrough
    
        mux1 False True False
     -> or2 (and2 (inv False) True) (and2 False False)
     -> or2 (and2  True True) (and2 False False)
     -> or2 True False
     -> True
     
        mux1 True True False
     -> or2 (and2 (inv True) True) (and2 True False)
     -> or2 (and2 False True) (and2 True False)
     -> or2 False False
     -> False
     
-}   

{-
    NOTE:
        Text mentions a 'Static' class to ensure fixed values
        but you can just use False and True
-}

-- convert Signal output (Bool's) to binary digit values
bitValue :: Bool -> Int
bitValue x = if x == False then 0 else 1

{-
    Exercise 3
    
    Use Haskell to test the half adder on the following test cases,
    and check that it produces the correct results.
    
    [Note: this is the truth table for half-adder given
           in the text, not what is shown in the Ex 3 table]
    
    halfAdd False False  -- 0 0
    halfAdd False True   -- 0 1
    halfAdd True  False  -- 0 1
    halfAdd True  True   -- 1 0  
-}
ab :: [(Bool,Bool)]
ab = [(False,False),(False,True),(True,False),(True,True)]

ex3 = map (\(a,b) -> (bitValue a, bitValue b)) 
          [ halfAdd a b | (a,b) <- ab]

{-
    Exercise 4
    
    Prove Theorem 106 using truth tables
    
    Theorem 16 states
     2 * bitValue c + bitValue s = bitValue a + bitValue b
    
    Ans:
        
        a       b     a  b  c  s     c   s       a   b
        ---------     ----  ----     -   -       -   -
        False False   0  0  0  0   2*0 + 0 = 0 = 0 + 0
        False True    0  1  0  1   2*0 + 1 = 1 = 0 + 1
        True  False   1  0  0  1   2*0 + 1 = 1 = 1 + 0
        True  True    1  1  1  0   2*1 + 0 = 2 = 1 + 1
-}
ex4 =    [2*a+b | (a,b) <- ex3] 
      == [ bitValue a + bitValue b | (a,b) <- ab]
      
{-
    Exercise 5
    
    Prove Theorem 107
    
    Theorem 107 states
        bitValue c' x 2 + bitValue s = bitValue a + bitValue b 
                                     + bitValue c

    Ans:
    
        a   b   c  c' s
        ---------  ----
        0   0   0  0  0    2*0 + 0 = 0 = 0 + 0 + 0
        0   0   1  0  1    2*0 + 1 = 1 = 0 + 0 + 1
        0   1   0  0  1    2*0 + 1 = 1 = 0 + 1 + 0
        0   1   1  1  0    2*1 + 0 = 2 = 0 + 1 + 1
        1   0   0  0  1    2*0 + 1 = 1 = 1 + 0 + 0
        1   0   1  1  0    2*1 + 0 = 2 = 1 + 0 + 1
        1   1   0  1  0    2*1 + 0 = 2 = 1 + 1 + 0
        1   1   1  1  1    2*1 + 1 = 3 = 1 + 1 + 1
    
-}
abcSums   = [bitValue a + bitValue b + bitValue c | (a,b,c) <- abc]
abcFA     = [fullAdd (a,b) c | (a,b,c) <- abc]
abcFASums = [ 2*(bitValue c) + bitValue s | (c,s) <- abcFA]
ex5 = abcSums == abcFASums

{-
    Exercise 6
    
    Work out the numeric value of the word [1, 0, 0, 1, 0]. Then check
    your result by using the computer to evaluate:

        wordValue [True,False,False,True,False]   

    Ans:
        word [1,0,0,1,0] = 1 * 2^4 + 1 * 2 = 18

-}
wordValue :: [Bool] -> Int
wordValue []     = 0
wordValue (x:xs) = 2^k * bitValue x + wordValue xs
    where k = length xs
    
ex6 = wordValue [True,False,False,True,False] == 18

{-
    Exercise 7
    
    Use Haskell to evaluate the add4 example, and check that the
    result matches the expected result.

-}
ex7 = add4 False [(False,True), (False,False), (True,False),
                  (True,False)]
{-
    Output:
    
    Main> ex7
    (False,[True,False,True,True])
    Main> 
-}    
{-
    Exercise 9
    
    Work out a test case using the ripple carry adder to calculate
    13+41=54, using 6-bit words. Test it using the computer.
    
    Ans:
        13 -> 001101 -> [False,False,True,True, False,True]
        41 -> 101001 -> [True, False,True,False,False,True]
        54 -> 110110 -> [True,True,False,True,True,False], no carry
-}
ex9 = rippleAdd False [(False,True),(False,False),(True,True),
                       (True,False),(False,False),(True,True)]  
      == (False, [True,True,False,True,True,False])

{-
    Exercise 10
    
    Define a full set of test cases for the circuit halfCmp, which
    compares two bits, and execute them using the computer.
    
    Ans:
        halfCmp (0,0) -> (0,1,0)
        halfCmp (0,1) -> (1,0,0)
        halfCmp (1,0) -> (0,0,1)
        halfCmp (1,1) -> (0,1,0)
-}      
halfCmp :: Signal a => (a,a) -> (a,a,a)
halfCmp (x,y) =
    (and2 (inv x) y,    -- x<y when x=0,y=1
    inv (xor x y),      -- x=y when x=0,y=0 or x=1,y=1
    and2 x (inv y))     -- x>y when x=1,y=0
        
ex10 = map halfCmp ab

{-
    Output:
    
    Main> ex10
    [(False,True,False),(True,False,False),
     (False,False,True),(False,True,False)]
    Main>     
-}
{-
    Exercise 11
    
    Define three test cases for the rippleCmp circuit, with a word
    size of three bits, demonstrating each of the three possible results. 
    Run your test cases on the computer

    Ans:
       x < y  (0,0,1) (0,1,0)  -> (1,0,0)
       x == y (0,1,0) (0,1,0)  -> (0,1,0)
       x > y  (1,0,0) (0,1,0)  -> (0,0,1)
       
-}
ex11a = rippleCmp $ zip [False,False,True]  [False,True,False]
ex11b = rippleCmp $ zip [False,True,False]  [False,True,False]
ex11c = rippleCmp $ zip [True, False,False] [False,True,False]

and3 :: Signal a => a -> a -> a -> a
and3 a b c = and2 a (and2 b c)         
        
fullCmp :: Signal a => (a,a,a) -> (a,a) -> (a,a,a)
fullCmp (lt,eq,gt) (x,y) =
    (or2 lt (and3 eq (inv x) y), -- <
    and2 eq (inv (xor x y)),     -- =
    or2 gt (and3 eq x (inv y)))  -- >        
    
-- assume the two are equal, and compare, reading left to right
--rippleCmp :: Signal a => [(a,a)] -> (a,a,a)
rippleCmp z = foldl fullCmp (False,True,False) z                     

{-
    Output:
    
        Main> ex11a
        (True,False,False)
        Main> ex11b
        (False,True,False)
        Main> ex11c
        (False,False,True)
        Main>     

-}