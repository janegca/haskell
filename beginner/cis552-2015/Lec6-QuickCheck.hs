module QuickCheck where
{-
    CIS 552: Advanced Programming (2015)
    Lecture 6 - QuickCheck
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/stub/QuickCheck.html
    
    Summary of what's covered:
    --------------------------

    QuickCheck is a Haskell library for property-based random testing
    QC properties are Boolean-valued functions that (partially) specify 
    the behavior of a function we want to test. E.g.

        prop_revapp_ok :: [Int] -> [Int]-> Bool prop_revapp_ok xs ys
            = reverse (xs ++ ys) == reverse ys ++ reverse xs

    More generally, QC provides a typeclass of Testable things, of which 
    Bool is one instance.

    QC also offers a number of combinators for building up "Properties," 
    which can be thought of as a generalized form of boolean expressions.

    The ==> combinator builds conditional properties:

        prop_revapp_ok :: [Int] -> [Int] -> Bool prop_revapp_ok xs ys 
            = reverse (xs ++ ys) == reverse ys ++ reverse xs

    The advantage of writing p ==> q instead of just not p || q is that 
    QC will keep track of the discard ratio of conditional tests that 
    succeed trivially because the precondition fails.

    To avoid too many discards, it is often necessary to build custom 
    generators for random test data.

        Create a fresh type (e.g., using newtype) for the data that is 
        to be generated, so that QC's default generators will not be used.

        Make this type an instance of the class Arbitrary

            class Arbitrary a where arbitrary :: Gen a

            where Gen a can be read as the type of generators for random 
                  values of type a.
                  
        Use QC's generator combinators to build an appropriate generator 
        for the distribution you have in mind: choose, oneOf, elements, 
        frequency, etc.

    (The monad combinators liftM, liftM2, and liftM3 are also useful.)
        Use the "debugging" combinators collect and classify to help tune 
        the distribution
    
-}
import Test.QuickCheck
import Control.Monad (liftM,liftM2,liftM3)
import Data.List (sort,insert)
import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

{-
    QuickCheck is a testing methodolgy based on 'type-directed property
    testing'. i.e. instead of writing unit tests, we write properties
    that we want functions to meet (have??) and then automatically
    generate random tests to verify or falsify the desired properties.
    
    The QuickCheck tool
        1. Forces you to think about what your code should do.
        2. Helps you find corner-cases
        3. Becomes part of the code as machine checkable documentation

    A QuickCheck property is basically a function that outputs a 
    boolean. There is a convention of using 'prop_' as a prefix
    for QC properties.
    
    Note that the example property show below uses 'concrete' types
    and NOT type variables; this is because QC has to generate
    random input and so needs to know which type will work. 
    
    Also note that only certain 'Testable' properties can be tested.
    
    To 'check' a propety we call it with the 'quickCheck' function.
-}
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

{-
    *QuickCheck> quickCheck prop_revapp
    +++ OK, passed 100 tests.
    *QuickCheck> 

    Here the tool generated and checked 100 inputs.
    
    If we want to generate more than 100 test inputs we can
    write a small utility function that will allow us to
    easily set the number of tests.
    
-}
-- utility function to set number of tests
quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

{-
    *QuickCheck> quickCheckN 1000 prop_revapp
    +++ OK, passed 1000 tests.
    *QuickCheck> 
-}
{-
    A more complex example
    ----------------------
    Lets say we have a quick sort function we'd like to check
    
-}
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs  = [y | y <- xs, y < x]   -- this is a "list comprehension"
        rhs  = [z | z <- xs, z > x]

{-
    We can try checking it out by hand:
    
        *QuickCheck> qsort [10,9..1]
        [1,2,3,4,5,6,7,8,9,10]
        *QuickCheck> qsort $ [2,4..20] ++ [1,3..11]
        [1,2,3,4,5,6,7,8,9,10,11,12,14,16,18,20]
        *QuickCheck>     

    and it looks ok but let's write a function that will actually
    check the result is ordered and test that property.

-}
isOrdered :: Ord a => [a] -> Bool
isOrdered []       = True
isOrdered [_]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered = isOrdered . qsort

{-
    *QuickCheck> quickCheckN 1000 prop_qsort_isOrdered
    +++ OK, passed 1000 tests.
    *QuickCheck>     

    We may want to check other properties
-}
-- check that repeated sortings do not change the list
prop_qsort_idemp ::  [Int] -> Bool 
prop_qsort_idemp xs = qsort (qsort xs) == qsort xs

{-
    *QuickCheck> quickCheck prop_qsort_idemp
    +++ OK, passed 100 tests.
    *QuickCheck> 

-}

-- we can add pre-conditions that the input must meet
-- i.e. that the list not be empty
-- Note also that the output the property function is now a
-- Property and not a Bool and the pre-condition is setup
-- using the 'implies' combinator (==>) provided by QuickCheck
-- which allows for the construction of rich properties
--
-- check that the head of the list is the minimum value
prop_qsort_nn_min    :: [Int] -> Property
prop_qsort_nn_min xs = 
  not (null xs) ==> head (qsort xs) == minimum xs

-- check that the last element in the list is the maximum value  
prop_qsort_nn_max    :: [Int] -> Property
prop_qsort_nn_max xs = 
  not (null xs) ==> last (qsort xs) == maximum xs

{-
    *QuickCheck> quickCheck prop_qsort_nn_min
    +++ OK, passed 100 tests.
    *QuickCheck> quickCheck prop_qsort_nn_max
    +++ OK, passed 100 tests.
    *QuickCheck> 

-}
-- testing against a 'model implementation' ie another function
-- that does what we need but which is less efficient or otherwise
-- unsuitable for deployment can reveal weaknesses
-- for example, we can test our 'qsort' against the built-in sort

prop_qsort_sort    :: [Int] -> Bool
prop_qsort_sort xs =  qsort xs == sort xs

{-
    *QuickCheck> quickCheckN 1000 prop_qsort_sort
    *** Failed! Falsifiable (after 209 tests and 2 shrinks): 
    [10,10]
    *QuickCheck> 

    OOPS! It failed. Why? Turns out qsort assumes there are no
    duplicate values in the list.  If we don't want to allow 
    duplicates we can stipulate that qsort only produce distinct
    values.
-}
isDistinct :: Eq a => [a] -> Bool
isDistinct []     = True
isDistinct (x:xs) = all (x /=) xs && isDistinct xs

prop_qsort_distinct :: [Int] -> Bool 
prop_qsort_distinct = isDistinct . qsort  

prop_qsort_distinct_sort :: [Int] -> Property
prop_qsort_distinct_sort xs = 
    isDistinct xs ==> qsort xs == sort xs
  
{-
    *QuickCheck> quickCheck prop_qsort_distinct
    +++ OK, passed 100 tests.
    *QuickCheck> quickCheck prop_qsort_distinct_sort
    +++ OK, passed 100 tests.
    *QuickCheck> 

    Now the check pass, but we need to be careful.
    When we add pre-conditions QuickCheck discards all the inputs
    that don't match the condition and churns away until it can
    produce the required number of tests; which isn't always
    possible.

    -- simplified version of standard insert method
    insert' x []                 = [x]
    insert' x (y:ys) | x > y     = x : y : ys
                    | otherwise  = y : insert' x ys
-}               
-- the standard insertion sort
isort :: Ord a => [a] -> [a]
isort = foldr insert []   

prop_isort_sort    :: [Int] -> Bool
prop_isort_sort xs = isort xs == sort xs

{-
    *QuickCheck> quickCheckN 1000 prop_isort_sort
    +++ OK, passed 1000 tests.

    This works because 'insert' preserves 'sortedness'
-}       
prop_insert_ordered'      :: Int -> [Int] -> Bool
prop_insert_ordered' x xs = isOrdered (insert x xs)

{-
    *QuickCheck> quickCheckN 1000 prop_insert_ordered'
    *** Failed! Falsifiable (after 7 tests and 3 shrinks): 
    0
    [-1,-2]
    *QuickCheck> insert 0 [-1,-2]
    [-1,-2,0]
    *QuickCheck> 

    This fails because the input is not ordered so the output
    is not ordered.
-}
prop_insert_ordered      :: Int -> [Int] -> Property 
prop_insert_ordered x xs = 
  isOrdered xs ==> isOrdered (insert x xs)
  
{-
    *QuickCheck> quickCheckN 1000 prop_insert_ordered
    *** Gave up! Passed only 577 tests.
    *QuickCheck>  

    This one gave up as not enough ordered lists are generated
    
    This also illustrates why it is better to use the p ==> q
    combinator than the (||) operator; the following will pass
    as input can be BOTH ordered and unordered.
-}  
prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
prop_insert_ordered_vacuous x xs = 
  not (isOrdered xs) || isOrdered (insert x xs)
  
  
{-
    *QuickCheck> quickCheckN 1000 prop_insert_ordered_vacuous
    +++ OK, passed 1000 tests.
    *QuickCheck> 

    We didn't really test for a check on ONLY ordered input.
    
    We can use combinators to 'collect' and 'classify' to
    investigate the distribution of test cases.

-}
prop_insert_ordered_vacuous' :: Int -> [Int] -> Property 
prop_insert_ordered_vacuous' x xs = 
  collect (length xs) $
  classify (isOrdered xs) "ord" $
  classify (not (isOrdered xs)) "not-ord" $
  not (isOrdered xs) || isOrdered (insert x xs)
  
{-
    *QuickCheck> quickCheck prop_insert_ordered_vacuous'
    +++ OK, passed 100 tests:
     5% 2, ord
     4% 7, not-ord
     4% 64, not-ord
     4% 20, not-ord
     4% 1, ord
     4% 0, ord
     3% 9, not-ord
     3% 8, not-ord
     3% 60, not-ord
     3% 56, not-ord
     ... [rest were all not-ord] ...

    From this we can see that only 13% (5+4+4) of the tests were
    actually ordered.
-}  
{-
    Generating Data
    ---------------
    QC uses a 'Gen a' monad to generate random values and the 
    class
    
        class Arbitrary a where
          arbitrary :: Gen a    
    
    to define types that can be randomly generated.
    
    If we want QC to generate random values of our own types
    we need to provide an Arbitrary instance for the type
    
    Combinators
    -----------
    QC also comes with a number of combinators that we can use
    to create custom instances of our own types.
    
        choose :: (System.Random.Random a) => (a, a) -> Gen a
            returns a random value from within the given interval
            
        elements :: [a] -> Gen a
            returns a generator that draws on values in the given list
            
        oneof :: [Gen a] -> Gen a
            allows us to randomly choose between generators
            
        frequency :: [(Int, Gen a)] -> Gen a
            allows us to build weighted combinations of individual
            generators
            
    To get an idea of what type of values will be generated,
    use 'sample'
        
        sample $ choose (0, 3)
        sample $ elements [10, 20..100]
        sample $ oneof [elements [10,20,30], choose (0,3)]
        
    Using these combinators, we can write list generators
-}
-- [Note: liftM2 is essentially 'fmap']
genList1 ::  (Arbitrary a) => Gen [a]
-- generates an infinite list
genList1 = liftM2 (:) arbitrary genList1

genList2 ::  (Arbitrary a) => Gen [a]
genList2 = oneof [ return []
                 , liftM2 (:) arbitrary genList2]
                 
{-
    *QuickCheck> sample (genList2 :: Gen [Int])
    [1,-1]
    []
    [0,1,3]
    []
    []
    []
    [-51]
    [73,198,45,210]
    [36]
    []
    [-188]
    *QuickCheck> 

    Better (not getting an infinite list) but getting alot of
    empty lists.
-}                 
-- improve the chances of not getting an empty list
genList3 ::  (Arbitrary a) => Gen [a]
genList3 = frequency [ (1, return [])
                     , (7, liftM2 (:) arbitrary genList3) ]
                     
{-

    *QuickCheck> sample (genList3 :: Gen [Int])
    [0,0,-1,0,0,0]
    [1,1,2,-2,-2,0,2]
    [1,0,1,-3,-1]
    [-1,8,-6,0,-7,-4,-3,-6,0,-8,-4,7,-8]
    [-2,6]
    [-53,44,-26,-31,-56,55,7,3,-26,56]
    [76,-74,54,41,45,-116,75]
    [66,-5,156]
    [-373,-247,-488,162,-956,-457,-105,446,834,749,738,744,-423,614,
     -643,196,-120,80,333,-388,-319,-45,579]
    [1993,1341,652,-1875,-1847,-589]
    [3530,-3513,1658,738,-3067,-17,-3679]
    *QuickCheck> 
    
    We can use the above to generate ordered lists.
-}    
genOrdList :: (Arbitrary a, Ord a) => Gen [a]
genOrdList = genList3 >>= return . sort

{-
    *QuickCheck> sample (genOrdList :: Gen [Int])
    [0]
    [-2,-2,-2,-2,-1,-1,-1,0,1,1,1,1,1,1,2,2]
    [-4,-3,-2,-2,-1,-1,-1,1,1,2,2,2,3,3,4]
    [-7,-4,-3,0,0,2,6]
    [9,9]
    [-64,-63,-56,-40,-30,-24,-24,-17,-5,3,7,9,11,11,17,35,56]
    [-122,-25]
    [-219,-79,-65,-51,-43,52,75,231,251]
    [-915,-841,-643,-637,-400,-398,-394,-300,-264,-152,-47,293,374,464,518,586,605,693,741,777,825,910]
    [-1982,-1336,-720,-664,-370,-11,50,138,945,1064,1240]
    [-3812,-2312,-251,-11,1985,2536]
    *QuickCheck> 

    We can check the output of a custom generator using 'forAll'
    
        *QuickCheck> quickCheck $ forAll genOrdList isOrdered 
        +++ OK, passed 100 tests.
        *QuickCheck> 
        
    Which means we can now properly check the insert property
-}       
prop_insert :: Int -> Property 
prop_insert x = forAll genOrdList $ \xs ->
  isOrdered xs && isOrdered (insert x xs)

{-
    *QuickCheck> quickCheck prop_insert
    +++ OK, passed 100 tests.
    *QuickCheck> 
-}
{-
    Using newtype for smarter test-case generation
    ----------------------------------------------
    If we don't want to use 'forall genOrdList' everywhere we
    want to check for properties on ordered lists we can 
    use 'newtype' to define a type that will wrap lists but
    use a different Arbitrary instance

-}
newtype OrdList a = OrdList [a] deriving (Eq, Ord, Show, Read)

instance (Ord a, Arbitrary a) => Arbitrary (OrdList a) where
  -- use genOrdList to create an Arbitrary list
  arbitrary = liftM OrdList genOrdList
  
{-
    *QuickCheck> sample (arbitrary :: Gen (OrdList Int))
    OrdList [-1,-1,0,0,1]
    OrdList [-2,-2,-2,-2,-2,-2,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,
              1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2]
    OrdList [-4,1,3,4]
    OrdList [-6,-4,-3,-1,6]
    ...

-}  
-- this lets us write a simpler version of our prop_insert function
prop_insert' :: Int -> OrdList Int -> Bool
prop_insert' x (OrdList xs) = isOrdered $ insert x xs

-- in fact, QuickCheck has a an ordered list type built-in
prop_insert'' :: Int -> OrderedList Int -> Bool
prop_insert'' x (Ordered xs) = isOrdered $ insert x xs

{-
    *QuickCheck> quickCheck prop_insert'
    +++ OK, passed 100 tests.
    *QuickCheck> quickCheck prop_insert''
    +++ OK, passed 100 tests.
    *QuickCheck>

-}
-- ---------------------------------------------------------------------
-- Case Study: Checking Compiler Optimizations
--      A small case study on how QC can be used to generate
--      structured data using a simple program expression.
--
-- An Expression can be one of a Var, Val or Op
-- Variables and Values are 'atomic' types
--      A Variable is essentially a String
--      A Value can be an Int or a Bool
--
-- There are a number of possible boolean operators, each with their
-- specific precedence.
--
-- Each custom type has a custom display (instance of Show)
--
-- ---------------------------------------------------------------------
data Expression = Var  Variable
                | Val  Value
                | Op   Bop Expression Expression
    deriving (Eq, Ord)
    
newtype Variable = V String deriving (Eq, Ord)    
 
data Value = IntVal  Int
           | BoolVal Bool
    deriving (Eq, Ord) 
 
data Bop = 
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool 
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Eq, Ord) 
        
-- custom instances for displaying output        
instance Show Variable where 
    show (V x) = x

instance Show Value where
    show (IntVal x)  = show x
    show (BoolVal x) = show x

instance Show Bop where 
    show b = case b of 
        Plus  -> "+"
        Minus -> "-"
        Times -> "*"
        Gt    -> ">" 
        Ge    -> ">="
        Lt    -> "<"
        Le    -> "<="
  
instance Show Expression where
    showsPrec d (Var x) = showsPrec d x
    showsPrec d (Val x) = showsPrec d x
    showsPrec d (Op bop e1 e2) = showParen (d > op_prec) $
      showsPrec (op_prec + 1) e1 . 
      showsPrec d bop . 
      showsPrec (op_prec + 1) e2
      where op_prec = precedence bop

-- set the precedence for each of the operators
precedence :: Bop -> Int
precedence Times = 8
precedence Plus  = 7
precedence Minus = 7
precedence Gt    = 6
precedence Ge    = 6
precedence Lt    = 6
precedence Le    = 6  
  
-- example expressions
expr1 = (Op Plus (Val (IntVal 1)) 
                 (Op Times (Val (IntVal 2)) (Val (IntVal 3))))
               
expr2 = (Op Times (Val (IntVal 1)) 
                  (Op Plus (Val (IntVal 2)) (Val (IntVal 3))))

ex1 = show expr1
ex2 = show expr2
     
{-
    *QuickCheck> ex1
    "1+2*3"
    *QuickCheck> ex2
    "1*(2+3)"
    *QuickCheck> 
-}     
-- To compute the value of an expression, we need a store---a map from 
-- variables to their values. We use a 'finite map' or dictionary
-- available from the Data.Map library (imported at the top of the file).
--
type Store = Map Variable Value

-- We also need a function to evaluate an operator
evalBop :: Bop -> Value -> Value -> Value
evalBop Plus  (IntVal v1) (IntVal v2) = IntVal (v1 + v2)
evalBop Times (IntVal v1) (IntVal v2) = IntVal (v1 * v2)
evalBop Minus (IntVal v1) (IntVal v2) = IntVal (v1 - v2)
evalBop Gt    (IntVal v1) (IntVal v2) = BoolVal (v1 > v2)
evalBop Ge    (IntVal v1) (IntVal v2) = BoolVal (v1 >= v2)
evalBop Lt    (IntVal v1) (IntVal v2) = BoolVal (v1 < v2)
evalBop Le    (IntVal v1) (IntVal v2) = BoolVal (v1 <= v2)
evalBop _     _           _           = IntVal 0

--  and another to evaluate an expression
eval :: Expression -> Store -> Value
eval (Var x) s = if Map.member x s then s Map.! x else IntVal 0
eval (Val a) s = a
eval (Op bop e1 e2) s = evalBop bop (eval e1 s) (eval e2 s)

-- Next we need a way to generate simple expressions
-- we'll start with a generator for variables, we assume the
-- variables are all uppercase values
instance Arbitrary Variable where 
    arbitrary = liftM V (elements $ map (:[]) ['A'..'Z'])
    
-- next is a generator for constant values which either be
-- Ints or Bools
instance Arbitrary Value where 
    arbitrary = oneof [ liftM IntVal  arbitrary, 
                        liftM BoolVal arbitrary ]    

-- and  a generator for Expressions and Bops
instance Arbitrary Bop where
    arbitrary = elements [Plus, Times, Minus, Gt, Ge, Lt, Le]                        

-- and a generator for Expressions    
-- we want to keep the expressions to a reasonable size
arbnE :: Int -> Gen Expression
arbnE n = frequency [ (1, liftM Var arbitrary), 
                      (1, liftM Val arbitrary),
                      (n, liftM3 Op arbitrary 
                          (arbnE (n `div` 2)) (arbnE (n `div` 2))) ]
 
{-  re-decalered further down
instance Arbitrary Expression where
  arbitrary = sized arbnE    
-}  
                  
-- and last, a generator for Store
instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
    arbitrary = liftM Map.fromList arbitrary 

-- setting Expression equivalence
(===) ::  Expression -> Expression -> Property
e1 === e2 = forAll arbitrary $ \st -> eval e1 st == eval e2 st

{-
    Checking An Optimization
    ------------------------
    Now we can check some compiler optimizations.
    An optimization can be viewed as a pair of programs:
        p_in        the input program
        p_out       the transformed program
    A transformation is correct iff p_in == p_out

-}    
prop_add_zero_elim :: Expression -> Property
prop_add_zero_elim e = (Op Plus e $ Val (IntVal 0)) === e 
 
{-
    *QuickCheck> quickCheck prop_add_zero_elim
    *** Failed! Falsifiable (after 5 tests): 
    True
    fromList [(N,1),(R,True)]
    *QuickCheck>     

    OOPs; forgot about boolean values; True + 0 evaluates to 0
    but True is just True
    
    We want to limit the checks to integers
-}
intE :: Gen Expression
intE = sized arbnEI 
  where 
    arbnEI 0 = oneof [ liftM Var arbitrary
                     , liftM (Val . IntVal) arbitrary ]
    arbnEI n = oneof [ liftM Var arbitrary
                     , liftM (Val . IntVal) arbitrary
                     , liftM2 (Op Plus)   (arbnEI n_by_2) (arbnEI n_by_2) 
                     , liftM2 (Op Times)  (arbnEI n_by_2) (arbnEI n_by_2) 
                     , liftM2 (Op Minus)  (arbnEI n_by_2) (arbnEI n_by_2) 
                     ]
               where n_by_2 = n `div` 2
               
prop_add_zero_elim' :: Property
prop_add_zero_elim' = 
  forAll intE $ \e -> (Op Plus e $ Val (IntVal 0)) === e

{-
    *QuickCheck> quickCheck prop_add_zero_elim'
    *** Failed! Falsifiable (after 16 tests): 
    P
    fromList [(B,False),(E,-36),(L,True),(O,True),(P,False),(T,71)]
    *QuickCheck> 

    Oops, this doesn't work either. Looks like we need to add
    some type information before we can elimination zero addition.
    
    QC comes with a test 'shrinking' mechanism that we can add
    to our Arbitrary Expression instance
-}  

-- code in online notes wouldn't compile, grabbed this from
-- https://github.com/x-y-z/2011fall/blob/master/
--      cis552/lecture.note/quickcheck.lhs#L1001
instance Arbitrary Expression where
  arbitrary = sized arbnE

  shrink (Op op e1 e2) = [e1,e2] 
                      ++ [Op op e1' e2 | e1' <- shrink e1]
                      ++ [Op op e1 e2' | e2' <- shrink e2]
                      -- ++ [liftM Val arbitrary]
                      
  shrink (Val i)       = liftM Val $ shrink i
  shrink _             = []
  
prop_const_prop ::  Variable -> Expression -> Store -> Bool
prop_const_prop x e s = eval (Var x) s' == eval e s' where
  s' = Map.insert x (eval e s) s

{-

    *QuickCheck> quickCheckN 1000 prop_const_prop
    *** Failed! Falsifiable (after 5 tests and 1 shrink): 
    C
    C>M
    fromList [(C,0),(S,-3),(T,2)]
    *QuickCheck>     

    Still failing; [don't think it's limited to integers ethier]
    Point was to show that while you can't test everything with
    QC you can test do quite alot
-}  