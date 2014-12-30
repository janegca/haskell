-- Informatics 1 Functional Programming
-- December 2014 Mock Exam
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#lectures

import Data.Char
import Data.List
import Control.Monad
import Test.QuickCheck


{- Question 1

    Suppose the playing cards in a standard deck are represented by 
    characters in the following way: '2' through '9' plus '0' (zero) 
    stand for the number cards, with '0' representing the 10, while the
    'A', 'K', 'Q' and 'J' stand for the face cards, i.e. the ace, king,
    queen and jack, respectively. Let's call these the 'card characters'.
    The other characters, including the lowercase letters 'a', 'k', 'q',
    and 'j', and the digit '1', are not used to represent cards.

-}
{- (a)

    Write a function 
            f :: String -> Bool 
    
    to test whether all card characters in a string represent face 
    cards. For example:
    
        f "ABCDE"     = True 
        f "none here" = True 
        f "4 Aces"    = False
        f "01234"     = False 
        f ""          = True 
        f "1 Ace"     = True

    Your function can use basic functions, list comprehension and 
    library functions, but not recursion. Credit may be given for 
    indicating how you have tested your function.
-}
cardChars :: String
cardChars = "023456789JQKA"

f :: String -> Bool
f xs =  and [ isUpper c | c <- xs, elem c cardChars ]

{- (b)
    Write a function 
            g :: String -> Bool 
            
    that behaves like f, this time using basic functions and recursion,
    but not library functions or list comprehension. Credit may be 
    given for indicating how you have tested your function.
-} 
g :: String -> Bool
g []     = True         
g (c:cs) | c `elem` cardChars && isUpper c = True && g cs
         | c `elem` cardChars              = False
         | otherwise                       = g cs

{- (c)

    Write a function 
            h :: String -> Bool 
            
    that behaves like f using one or  more of the following 
    higher-order functions:
    
        map :: (a -> b) -> [a] -> [b]
        filter :: (a -> Bool) -> [a] -> [a]
        foldr :: (a -> b -> b) -> b -> [a] -> b
    
    You can also use basic functions, but not other library functions, 
    recursion or list comprehension. Credit may be given for indicating 
    how you have tested your function.

-}         
h :: String -> Bool
h = foldr (&&) True . map (isUpper) . filter (`elem` cardChars)
         
testq1 fn = fn "ABCDE"     == True &&
            fn "none here" == True &&
            fn "4 Aces"    == False &&
            fn "01234"     == False &&
            fn ""          == True &&
            fn "1 Ace"     == True
            
test1a = testq1 f            
test1b = testq1 g
test1c = testq1 h

-- Question 2 ---------------------------------------------------------
{- (a)

    Write a polymorphic function 
            t :: [a] -> [a] 
            
    that duplicates every other item in a list. The result should 
    contain the first item once, the second twice, the third once, 
    the fourth twice, and so on. For example,
    
        t "abcdefg" = "abbcddeffg"
        t [1,2,3,4] = [1,2,2,3,4,4]
        t "" = ""
    
    Your definition may use basic functions, list comprehension, and 
    library functions, but not recursion. Credit may be given for 
    indicating how you have tested your function.

-}
-- provided solution
t :: [a] -> [a]
t xs = concat [ if odd i then [x] else [x,x] | (x,i) <- zip xs [1..] ]
     
{- (b)

    Write a second function 
            u :: [a] -> [a] 
            
    that behaves like t, this time using basic functions and recursion, 
    but not list comprehension or library functions. Credit may be 
    given for indicating how you have tested your function.

-}     
u :: [a] -> [a]
u []       = []
u [x]      = [x]
u (x:y:zs) = x : y : y : u zs     
     
     
test2b = u "abcdefg" == "abbcddeffg" &&
         u [1,2,3,4] == [1,2,2,3,4,4] &&
         u "" == ""
               
-- provided solutions for quick tests
prop_tu_string :: String -> Bool
prop_tu_string str = t str == u str

prop_tu_intlist :: [Int] -> Bool
prop_tu_intlist xs = t xs == u xs
               
               
-- Question 3 ---------------------------------------------------------

data  Proposition  =   Var String
		   |   F
		   |   T
		   |   Not Proposition
		   |   Proposition :|: Proposition
		   |   Proposition :&: Proposition
		   deriving (Eq, Ord, Show)

instance Arbitrary Proposition where
  arbitrary = sized expr
    where
      expr 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"])]
      expr n | n > 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"]),
               liftM Not (expr (n-1)),
               liftM2 (:&:) (expr (n `div` 2)) (expr (n `div` 2)),
               liftM2 (:|:) (expr (n `div` 2)) (expr (n `div` 2))]

{- (a)

    Write a function 
        isNorm :: Proposition -> Bool 
    
    that returns true when a proposition is in negation normal form. 
    A proposition is in negation normal form if the only occurrences 
    of logical negation (Not) are applied to variables. For example,
    
        isNorm (Var "p" :&: Not (Var "q"))       = True
        isNorm (Not (Var "p" :|: Var "q"))       = False
        isNorm (Not (Not (Var "p")) :|: Not T)   = False
        isNorm (Not (Var "p" :&: Not (Var "q"))) = False
    
    Credit may be given for indicating how you have tested your 
    function.

-}               
isNorm :: Proposition -> Bool
isNorm (Not (Var x)) = True
isNorm (Not p)       = False
isNorm (a :&: b)     = isNorm a && isNorm b
isNorm (a :|: b)     = isNorm a && isNorm b
isNorm (Var x)       = True
--isNorm p             = False      -- wrong
isNorm T             = True
isNorm F             = True

test3a = isNorm (Var "p" :&: Not (Var "q"))       == True &&
         isNorm (Not (Var "p" :|: Var "q"))       == False &&
         isNorm (Not (Not (Var "p")) :|: Not T)   == False &&
         isNorm (Not (Var "p" :&: Not (Var "q"))) == False
         
{- (b)

    Write a function 
            norm :: Proposition -> Proposition 
            
    that converts a proposition to an equivalent proposition in 
    negation normal form. A proposition may be converted to normal 
    form by repeated application of the following equivalences:
    
            Not F <-> T
            Not T <-> F
            Not (Not p) <-> p
            Not (p :|: q) <-> Not p :&: Not q
            Not (p :&: q) <-> Not p :|: Not q
    
    For example,
        norm (Var "p" :&: Not (Var "q"))
        = (Var "p" :&: Not (Var "q"))
        
        norm (Not (Var "p" :|: Var "q"))
        = Not (Var "p") :&: Not (Var "q")
        
        norm (Not (Not (Var "p")) :|: Not T)
        = (Var "p" :|: F)
        
        norm (Not (Var "p" :&: Not (Var "q")))
        = Not (Var "p") :|: Var "q"

    Credit may be given for indicating how you have tested your 
    function.

-}         
-- revised based on provided solution
norm :: Proposition -> Proposition
norm (Not (Var x))   = Not (Var x)              -- soln
norm (p :&: q)       = norm p :&: norm q        -- ok
norm (p :|: q)       = norm p :|: norm q        -- ok
norm (Not F)         = T                        -- ok
norm (Not T)         = F                        -- ok
norm (Not (Not p))   = norm p                   -- ok
norm (Not (p :|: q)) = norm (Not p) :&: norm (Not q) -- revised
norm (Not (p :&: q)) = norm (Not p) :|: norm (Not q) -- revised
norm p               = p

test3b =
      norm (Var "p" :&: Not (Var "q")) == (Var "p" :&: Not (Var "q"))
   && norm (Not (Var "p" :|: Var "q")) == Not (Var "p") :&: Not (Var "q")
   && norm (Not (Not (Var "p")) :|: Not T) == (Var "p" :|: F)
   && norm (Not (Var "p" :&: Not (Var "q"))) == Not (Var "p") :|: Var "q"

prop_q3 :: Proposition -> Bool
prop_q3 p  =  isNorm (norm p)
   