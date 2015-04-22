{-
    CIS 552: Advanced Programming (2015)
    Homework 3 - QuickCheck
    
    Source:
    https://www.seas.upenn.edu/~cis552/current/hw/hw03/index.html
-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances #-}


module Main where

import Data.List
import Data.Maybe

-- These two import declarations say: (1) import the type Map from the
-- module Data.Map as an unqualified identifier, so that we can use
-- just `Map a b` as a type; and (2) import everything else from
-- Data.Map as qualified identifiers, written Map.member, Map.!, etc.
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Test.QuickCheck
import System.Random

------------------------------------------------------------------------------
-- Problem 1: QuickCheck properties for lists
-- In this problem, you'll be writing QuickCheck properties to specify 
-- various list manipulation functions. For each property you write, you 
-- should also include a buggy implementation of the tested function that 
-- does not satisfy the property you wrote. We'll be asking you to write 
-- your properties in a very particular way.

prop_const' :: Eq a => a -> a -> Bool
prop_const' a b = const a b == a 

-- *Main> quickCheck (prop_const' :: Char -> Char -> Bool)

data Undefined
instance Testable Undefined where
  property = error "Unimplemented property"

-- using a parameter (here, const'), you can pass in variations
-- of the function to be used in the test  
prop_const :: Eq a => (a -> a -> a) -> a -> a -> Bool
prop_const const' a b = const' a b == a

const_bug :: a -> a -> a
const_bug _ b = b -- Oops: this returns the *second* arg, not the first.

{-
    Example of passing in the test function to be used

    *Main> quickCheck (prop_const const :: Char -> Char -> Bool)
    +++ OK, passed 100 tests.
    
    *Main> quickCheck (prop_const const_bug :: Char -> Char -> Bool)
    *** Failed! Falsifiable (after 1 test and 2 shrinks): 
    'a'
    'b'
    *Main> 
-}

{- Part a

    Define a property showing that minimum really returns the smallest element in a list; also, write a buggy implementation of minimum that doesn't satisfy your property. (Don't forget to fill in the type signature for prop_minimum!)
-}

prop_minimum :: Ord a => ([a] -> a) -> [a] -> Property
prop_minimum minimum' xs = 
    not (null xs) ==> minimum' xs == head (sort xs)

minimum_bug :: Ord a => [a] -> a
minimum_bug = head

{-

    *Main> quickCheck (prop_minimum minimum :: [Char] -> Property)
    +++ OK, passed 100 tests.
    *Main> quickCheck (prop_minimum minimum_bug :: [Char] -> Property)
    *** Failed! Falsifiable (after 5 tests and 3 shrinks): 
    "aA"
    *Main> 

-}
{-
    Part b

    Define a property specifying the replicate function from the standard library, and a buggy implementation that violates this spec. Recall that replicate k x is a list containing k copies of x.
    
    You will first need to define a newtype for generating small non-negative numbers

    Ref: https://github.com/Yuhta/cis552/hw3/Main.hs
-}
newtype SmallNonNeg a = SmallNonNeg a deriving (Eq, Ord, Show, Read)
 
instance (Num a, Random a, Arbitrary a) => Arbitrary (SmallNonNeg a) where
    arbitrary              = SmallNonNeg `liftM` choose (0,10)
    shrink (SmallNonNeg n) = SmallNonNeg `liftM` (shrink n) 

prop_replicate :: (Int -> a -> [a]) -> SmallNonNeg Int -> a -> Bool
prop_replicate replicate' (SmallNonNeg k) x = 
    length (replicate' k x) == k

replicate_bug :: Int -> a -> [a]
replicate_bug k x = replicate (k-1) x

{-
    *Main> quickCheck (prop_replicate replicate :: SmallNonNeg Int -> Char -> Bool)
    +++ OK, passed 100 tests.

    *Main> quickCheck (prop_replicate replicate_bug :: SmallNonNeg Int -> Char -> Bool)
    *** Failed! Falsifiable (after 1 test and 5 shrinks): 
    SmallNonNeg 1
    'a'
    *Main>     

-}
{-
    Part c

    Define two properties specifying group; the first one should say that
    "the concatenation of the result is equal to the argument", and the 
    second should say that "each sublist in the result is non-empty and 
    contains only equal elements". Also write a buggy version of group 
    that violates both of them.

-}

prop_group_1 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_1 group' xs = concat ys == xs where ys = group' xs

prop_group_2 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_2 group' xs = all valid $ group' xs 
    where valid ys = not (null ys) && all (== head ys) ys

group_bug :: Eq a => [a] -> [[a]]
group_bug xs = badGrp xs : group xs
    where badGrp (x:y:zs) = [x,y] ++ zs
          badGrp  _       = []
          
-- test to see if 'all' is really mapping to individual elements     
-- and not just treating the list as a single String     
test :: Bool
test = all valid ["M","i","ss","i","xy","ss","i","pp","i"]
    where valid ys = not (null ys) && all (== head ys) ys
          
{-
    *Main> quickCheck (prop_group_1 group :: String -> Bool)
    +++ OK, passed 100 tests.
    *Main> 
    
    *Main> quickCheck (prop_group_1 group_bug :: String -> Bool)
    *** Failed! Falsifiable (after 3 tests and 2 shrinks): 
    "aa"
    *Main>    
    
    *Main> quickCheck (prop_group_2 group :: String -> Bool)
    +++ OK, passed 100 tests.
    *Main>

    *Main> quickCheck (prop_group_2 group_bug :: String -> Bool)
    *** Failed! Falsifiable (after 1 test): 
    ""
    *Main> 

-}
{-
    Part d

    Write two interesting properties about reverse. Write two different 
    buggy versions, one which violates each property.

-}

-- reversed list has the same length as the original list
prop_reverse_1 :: ([a] -> [a]) -> [a] -> Bool
prop_reverse_1 reverse' xs = length (reverse' xs) == length xs

-- the reverse of a reversed list is equal to the original list
prop_reverse_2 :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_2 reverse' xs = (reverse' (reverse' xs)) == xs

reverse_bug_1 :: [a] -> [a]
reverse_bug_1 xs = reverse (tail xs)

reverse_bug_2 :: [a] -> [a]
reverse_bug_2 xs = reverse (init xs)

{-
    *Main> quickCheck (prop_reverse_1 reverse :: String -> Bool)
    +++ OK, passed 100 tests.
    *Main> quickCheck (prop_reverse_2 reverse :: String -> Bool)
    +++ OK, passed 100 tests.
    *Main> 
    
    *Main> quickCheck (prop_reverse_1 reverse_bug_1 :: String -> Bool)
    *** Failed! Exception: 'Prelude.tail: empty list' (after 1 test): 
    ""
    *Main> quickCheck (prop_reverse_2 reverse_bug_2 :: String -> Bool)
    *** Failed! Exception: 'Prelude.init: empty list' (after 1 test): 
    ""
    *Main>     

-}

listPropertiesMain :: IO ()
listPropertiesMain = do
  let qcName name prop = do
        putStr $ name ++ ": "
        quickCheck prop
  
  putStrLn "The following tests should all succeed:"
  qcName "const"     $ prop_const     (const     :: Char -> Char -> Char)
  qcName "minimum"   $ prop_minimum   (minimum   :: [Char] -> Char)
  qcName "replicate" $ prop_replicate (replicate :: Int -> Char -> [Char])
  qcName "group_1"   $ prop_group_1   (group     :: [Char] -> [[Char]])
  qcName "group_2"   $ prop_group_2   (group     :: [Char] -> [[Char]])
  qcName "reverse_1" $ prop_reverse_1 (reverse   :: [Char] -> [Char])
  qcName "reverse_2" $ prop_reverse_2 (reverse   :: [Char] -> [Char])

  putStrLn ""

  putStrLn "The following tests should all fail:"
  qcName "const"     $ prop_const     (const_bug     :: Char -> Char -> Char)
  qcName "minimum"   $ prop_minimum   (minimum_bug   :: [Char] -> Char)
  qcName "replicate" $ prop_replicate (replicate_bug :: Int -> Char -> [Char])
  qcName "group_1"   $ prop_group_1   (group_bug     :: [Char] -> [[Char]])
  qcName "group_2"   $ prop_group_2   (group_bug     :: [Char] -> [[Char]])
  qcName "reverse_1" $ prop_reverse_1 (reverse_bug_1 :: [Char] -> [Char])
  qcName "reverse_2" $ prop_reverse_2 (reverse_bug_2 :: [Char] -> [Char])

--------------------------------------------------------------------------
-- Problem 2: Using QuickCheck to debug a SAT solver
--      A CNF formula is satisfied by a valuation if the valuation makes
--      the formula true.
--
-- Given ref: https://en.wikipedia.org/wiki/DPLL_algorithm
--            (the second reference link didn't work)
-- Map: https://hackage.haskell.org/package/containers-0.5.6.3/docs/
--                    Data-Map-Strict.html
--
-- Solution Ref: https://github.com/Yuhta/cis552/hw3/Main.hs
-------------------------------------------------------------------------- 
-- Basic types

-- | An expression in CNF (conjunctive normal form) is a conjunction [AND]
-- of clauses
type CNF = [ Clause ]

-- | A clause is a disjunction [OR] of a number of literals
type Clause = [ Lit ]

-- | A literal is either a positive or a negative variable
data Lit = Lit Bool Var deriving (Eq, Ord, Show)

instance Arbitrary Lit where
  arbitrary = liftM2 Lit arbitrary arbitrary
  shrink (Lit b v) = map (flip Lit v) (shrink b) ++
                     map (Lit b) (shrink v)

-- | Variables
data Var = A | B | C | D | E | F 
  deriving (Read, Eq, Ord, Show, Enum, Bounded)

var :: Lit -> Var
var (Lit _ x) = x

instance Arbitrary Var where
  arbitrary = arbitraryBoundedEnum
  shrink A = []
  shrink x = enumFromTo A $ pred x

-- | Is the literal positive?
isPos :: Lit -> Bool
isPos (Lit b _) = b

-- | invert the polarity of a literal
invert :: Lit -> Lit
invert (Lit b x) = Lit (not b) x

-- Example formula in problem description: 
--          (A ∨ B ∨ ¬C) ∧ (¬A) ∧ (¬B ∨ ¬C)
--  [doesn't appear to match below, which looks like it reads: 
--         (A ∨ B ∨ C) ∧ (¬A) ∧ (¬B ∨ C) ]
exampleFormula :: CNF
exampleFormula = [[Lit True A, Lit True B, Lit True C],
                  [Lit False A],
                  [Lit False B, Lit True C]]

unSatFormula :: CNF
unSatFormula = [[Lit True A],
                [Lit False A]]

-- | Assignments of values to (some) variables
type Valuation = Map Var Bool

emptyValuation :: Valuation
emptyValuation = Map.empty

-- create a Map from a list of (variable, value) elements
-- a Map contains distinct (unique) keys; uses the last value
-- given if duplicates are added
-- e.g. fromList [(A,True), (A, False)]  -> [(A,False)]
--      fromList [(A,False),(A,True)]    -> [(A,True)]
fromList :: [(Var,Bool)] -> Valuation
fromList = Map.fromList

exampleValuation :: Valuation
exampleValuation = fromList [(A,False), (B,True), (C,True)]

-- is the value of a variable the same as its value in a valuation?
-- eg  litSatisfied exampleValuation (Lit True B)  -> True
--     litSatisfied exampleValuation (Lit True A)  -> False
-- Note: Map.! k - finds the value for the given key
litSatisfied :: Valuation -> Lit -> Bool
litSatisfied a (Lit b v) = Map.member v a && (b == a Map.! v)

-- a CNF formula is satisfied by a valuation if the valuation
-- makes the formula true
satisfiedBy :: CNF -> Valuation -> Bool
satisfiedBy p a = all (any $ litSatisfied a) p

prop_satisfiedBy :: Bool
prop_satisfiedBy = exampleFormula `satisfiedBy` exampleValuation

{-
    *Main> quickCheck prop_satisfiedBy
    +++ OK, passed 1 tests.
    *Main> 
-}
{-
    Valuations support two main operations: extending them with a new 
    binding and checking what is the value of a variable. Please define 
    them. (You'll need to refer to the documentation for the Data.Map 
    module.)
    
-}

extend :: Var -> Bool -> Valuation -> Valuation
extend = Map.insert

value :: Var -> Valuation -> Maybe Bool
value = Map.lookup

---------------------------------------------------------------------------
-- Simple SAT Solver
--  Tries all possible valuations and stops when it finds a 
--  satisfying valuation.
--
-- INCOMPLETE 

allVars :: [Var]
allVars = [minBound .. maxBound]    -- enumeration of Var type

-- create two versions of each variable, one True, one False
-- and compute every possible variation with all other variable values
allValuations :: [Valuation]
allValuations = allValuations' allVars where
  allValuations' []     = [emptyValuation]
  allValuations' (v:vs) = [ extend v b x
                          | b <- [False, True]
                          , x <- allValuations' vs ]

prop_allValuations :: Bool
prop_allValuations = length allValuations == 2 ^ length allVars
                     && allElementsDistinct allValuations
                     
allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct []     = True
allElementsDistinct (x:xs) = all (x /=) xs &&
                             allElementsDistinct xs
                             
{-
    *Main> quickCheck prop_allValuations
    +++ OK, passed 1 tests.
    *Main>
-}                                                  

-- search the list of all valuations for one that satisfies 
-- the given formula                             
sat0 :: CNF -> Maybe Valuation
sat0 p = find (p `satisfiedBy` ) allValuations

prop_satResultCorrect :: (CNF -> Maybe Valuation) -> Property
prop_satResultCorrect solver = 
  forAll arbitrary $ \p -> case solver p of
                               Just a  -> p `satisfiedBy` a
                               Nothing -> True
{-
    *Main> quickCheck $ prop_satResultCorrect sat0
    +++ OK, passed 100 tests.
    *Main> 
-}                               
---------------------------------------------------------------------------
-- Instantiation
--  The simple solver is inefficient; for one thing, it evaluates
--  the whole CNF for each new valuation
--  One thing we can do is choose a truth value for a variable
--  and then reduce the formula to reflect that choice
--  e.g.  given (A ∨ ¬B) ∧ (¬A ∨ B ∨ C)
--        if we instantiate A (set A) to True then the formula becomes
--                 (True ∨ ¬B) ∧ (False ∨ B ∨ C), 
--        which can be simplified to 
--              True ∧ (False ∨ B ∨ C) -> (B ∨ C)

instantiate :: CNF -> Var -> Bool -> CNF
instantiate [] _ _         = []
instantiate (c:cs) var val = instantiate' c : instantiate cs var val
    where 
        instantiate' ( p@(Lit _ v) : vs)
            -- drop if the changed variable is True
            |  var == v && isPos p == val  = instantiate' vs
            | otherwise = p : instantiate' vs
        instantiate' [] = []

prop_instantiate :: CNF -> Var -> Bool
prop_instantiate s v = undefined


sat1 :: CNF -> Maybe Valuation
sat1 = sat where
  sat = undefined

prop_sat1 :: CNF -> Bool
prop_sat1 s = (isJust $ sat1 s) == (isJust $ sat0 s)

--------------------------------------------------------------------------- 
-- Unit propagation

simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyUnitClause s) returns Nothing, then there 
--    are no remaining unit clauses in s.
-- 2) If it returns (Just s'), then s' is satisfiable iff s is.
prop_simplifyUnitClause :: CNF -> Bool
prop_simplifyUnitClause = undefined

unitClauses :: CNF -> [Lit]
unitClauses = undefined

simplifyUnitClause = undefined

sat2 :: CNF -> Maybe Valuation
sat2 = sat where
  sat = undefined

prop_sat2 :: CNF -> Bool
prop_sat2 s = (isJust $ sat2 s) == (isJust $ sat0 s)

--------------------------------------------------------------------------- 
-- Pure literal elimination

simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyPureLiteral s) returns Nothing, then there 
--    are no remaining pure literals in s
-- 2) If it returns (Just s'), then s' is satisfiable iff s is
prop_simplifyPureLiteral :: CNF -> Bool
prop_simplifyPureLiteral = undefined

literals :: CNF -> [Lit]
literals = concat 
  
data Purity = Impure | Pure Bool

pureLiterals :: CNF -> [(Var,Bool)]
pureLiterals = undefined

simplifyPureLiteral = undefined

-- The final DPLL algorithm:
dpll :: CNF -> Maybe Valuation
dpll = sat where
  sat = undefined

prop_dpll :: CNF -> Bool
prop_dpll s = (isJust $ dpll s) == (isJust $ sat0 s)

------------------------------------------------------------------------------
-- Using QC as a SAT solver

prop_isSatisfiable :: CNF -> Property
prop_isSatisfiable = undefined

------------------------------------------------------------------------------
-- All the tests in one convenient place:

main :: IO ()
main = quickCheck $    prop_satisfiedBy
                  .&&. prop_satResultCorrect sat0
                  .&&. prop_instantiate
                  .&&. prop_sat1
                  .&&. prop_satResultCorrect sat1
                  .&&. prop_simplifyUnitClause
                  .&&. prop_sat2
                  .&&. prop_satResultCorrect sat2
                  .&&. prop_simplifyPureLiteral
                  .&&. prop_dpll
                  .&&. prop_satResultCorrect dpll

