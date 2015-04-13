{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-
    CIS 552: Advanced Programming (2015)
    Homework 2 - Datatypes and Trees
    
    The goal of this homework assignment is practice with user-defined 
    datatypes and trees in Haskell.

    This homework is composed of three files: two support files 
    XMLTypes.hs and Play.hs, plus the main part of the assignment 
    (this file). For testing, you will also need the file sample.html.
    To complete the homework, you should edit only the file Main.hs  

    To be able to load or compile this assignment you will first need to install the Gloss graphics library. You can do so using the command:

    cabal install gloss    
    
-}
module Main where
import Prelude hiding (takeWhile,all)
import Test.HUnit      -- unit test support
import Graphics.Gloss hiding (play) -- graphics library for problem 2
-- import GlossStub    -- "stubbed" version of gloss if you have trouble
import XMLTypes        -- support file for problem 3 (provided)
import Play            -- support file for problem 3 (provided)

doTests :: IO ()
doTests = do 
  _ <- runTestTT $ TestList [ test0, test1, test2, test3 ]
  return ()

main :: IO ()
main = do 
       doTests
       -- drawCircles   --- graphics demo, change to drawTree for your HW
       drawSierpinski  
       return ()

----------------------------------------------------------------------

-- 2 (a)

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
intersperse :: a -> [a] -> [a]
intersperse a = foldr insrt []
    where
        insrt x [] = [x]
        insrt x xs = x : a : xs

t2a :: Test
t2a = "2a" ~: intersperse ',' "abcde" ~?= "a,b,c,d,e"

-- 2 (b)

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 
invert :: [(a,b)] -> [(b,a)]
invert = map (\(a,b) -> (b,a))

t2b :: Test
t2b = "2b" ~: invert [("a",1),("a",2)] ~?= [(1,"a"),(2,"a")] 
 

-- 2 (c)

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr step []
    where
        step x xs = if p x then x:xs else []
             
t2c, t2ca, t2cb :: Test
t2c = "2c"   ~: takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2]
t2ca = "2ca" ~: takeWhile (< 9) [1,2,3] ~?= [1,2,3]
t2cb = "2cb" ~: takeWhile (< 0) [1,2,3] ~?= []

 
-- 2 (d)

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3
--
-- [Note: used, filter instead of map or foldr]
find :: (a -> Bool) -> [a] -> Maybe a
find p xs = if null a then Nothing else Just (head a)
    where a = filter p xs
        
t2d, t2da :: Test
t2d  = "2d"   ~: find odd [0,2,3,4] ~?= Just 3
t2da = "2da" ~: find odd [] ~?= Nothing
 
-- 2 (e)

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

t2e, t2ea, t2eb :: Test
t2e  = "3e"  ~: all odd [1,2,3] ~?= False
t2ea = "3ea" ~: all odd []      ~?= True
t2eb = "3eb" ~: all odd [1,3,5] ~?= True

test2 :: Test
test2 = TestList [t2a, t2b, t2c, t2ca, t2cb, t2d, t2da, t2e]

----------------------------------------------------------------------

-- | a basic tree data structure
data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf     = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2) 

-- 0 (a)

-- The invertTree function takes a tree of pairs and returns a new tree 
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Leaf Leaf) returns Branch (1,"a") Leaf Leaf
invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = mapTree (\(a,b) -> (b,a))

t0a :: Test
t0a = "0a" ~: invertTree (Branch ("a",1) Leaf Leaf) 
           ~?= Branch (1,"a") Leaf Leaf

-- 0 (b)

-- takeWhileTree, applied to a predicate p and a tree t, 
-- returns the largest prefix tree of t  (possibly empty) 
-- where all elements satisfy p. 
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

--  takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Leaf Leaf) Leaf
--  takeWhileTree (< 9) tree1  returns tree1
--  takeWhileTree (< 0) tree1  returns Leaf

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p = foldTree Leaf 
                    (\e t1 t2 -> if p e then Branch e t1 t2 else Leaf)
           
t0b, t0ba, t0bb :: Test
t0b = "0b" ~: takeWhileTree (< 3) tree1 
           ~?= Branch 1 (Branch 2 Leaf Leaf) Leaf
t0ba = "0ba" ~: takeWhileTree (< 9) tree1  ~?= tree1
t0bb = "0bb" ~: takeWhileTree (< 0) tree1  ~?= Leaf

-- 0 (c) 
 
-- allTree pred tree returns False if any element of tree 
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = foldTree True (\e t1 t2 -> (p e) && t1 && t2)

t0c :: Test
t0c = "0c" ~: allTree odd tree1 ~?= False
 
-- 0 (d) WARNING: This one is a bit tricky!  (Hint: the value
-- *returned* by foldTree can itself be a function.)

-- map2Tree f xs ys returns the tree obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one branch is longer than the other, then the extra elements 
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf)
--        should return (Branch 4 Leaf Leaf)
map2Tree :: (a -> a -> b) -> Tree a -> Tree a -> Tree b
map2Tree _ Leaf _ = Leaf
map2Tree _ _ Leaf = Leaf
map2Tree f (Branch e1 a1 a2) (Branch e2 b1 b2)
    = Branch (f e1 e2) (map2Tree f a1 a2) (map2Tree f b1 b2)
    
t0d :: Test
t0d = "0d" ~: map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) 
                                          (Branch 3 Leaf Leaf)
           ~?= (Branch 4 Leaf Leaf)

-- 0 (e) 

-- zipTree takes two trees and returns a tree of corresponding pairs. If
-- one input branch is smaller, excess elements of the longer branch are
-- discarded.
-- for example:  
--    zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) (Branch True Leaf Leaf) returns 
--            (Branch (1,True) Leaf Leaf)

-- To use foldTree, you'll need to think about this one in
-- the same way as part (d).
zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree Leaf _ = Leaf
zipTree _ Leaf = Leaf
zipTree (Branch e1 a1 a2) (Branch e2 b1 b2) 
    = Branch (e1,e2) (zipTree a1 b1) (zipTree a2 b2)
    
t0e :: Test
t0e = "0e" ~: zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) 
                       (Branch True Leaf Leaf)
           ~?= (Branch (1,True) Leaf Leaf)

test0 :: Test
test0 = TestList [ t0a, t0b, t0ba, t0bb, t0c, t0d, t0e] 

----------------------------------------------------------------------

-- | Display a gloss picture, given the name of the window, size of the 
-- window, position, background color and the picture itself. 
displayInWindow :: String -> (Int, Int) -> (Int, Int) 
                          -> Color -> Picture -> IO ()
displayInWindow x y z = display (InWindow x y z)

-- | a picture composed of concentric circles
circles :: Picture
circles = pictures (reverse colorCircles) where
   -- a list of circle pictures with increasing radii
   bwCircles :: [Picture]
   bwCircles = map (\f -> circleSolid (25.0 * f)) [1.0 ..]  
   -- a list of builtin colors
   colors :: [Color]
   colors    = [red, blue, green, cyan, magenta, yellow]
   -- a list of colored circles
   colorCircles :: [Picture]
   colorCircles = zipWith color colors bwCircles

-- | draw the concentric circle picture in a window
-- this variable is an "action" of type IO (). Running the action
-- in the main program will open a window (of size (600,600)) and 
-- display the circles.

drawCircles :: IO ()
drawCircles = displayInWindow "Circles" (600,600) (10,10) black circles

-- | a right triangle at position `x` `y` with side length `size`
triangle :: Float -> Float -> Float -> Picture
triangle x y size = line [(x,y), (x+size, y), (x, y-size), (x,y)]

minSize :: Float
minSize = 8.0

-- | a sierpinski triangle
sierpinski :: Float -> Float -> Float -> [ Picture ]
sierpinski x y size = 
  if size <= minSize
  then [ triangle x y size ] 
  else let size2 = size / 2 
       in sierpinski x y size2 ++ 
          sierpinski x (y-size2) size2 ++
          sierpinski (x+size2) y size2

-- | the action to draw the triangle on the screen
drawSierpinski :: IO ()
drawSierpinski = 
   displayInWindow "Sierpinski" (600,600) (10,10) white sierpinskiPicture where
      sierpinskiPicture = color blue (pictures (sierpinski 0 0 256))

-- 1 (a)
-- Given a ration for the child sizes and an initial size, create
-- a tree structure storing the size of each branch
calcSize :: Float -> Float -> Tree Float
calcSize r s | s < minSize = Leaf
             | otherwise   = Branch s (calcSize r (s*r)) 
                                      (calcSize r (s*r))

t1a :: Test
t1a = "1a" ~: calcSize 0.5 25 ~=?
         Branch 25.0 (Branch 12.5 Leaf Leaf) (Branch 12.5 Leaf Leaf)

-- 1 (b)
-- The function fractal delta angle x y sizeTree should return a tree of
-- pictures, where the root of the tree starts at position (x,y) and 
-- draws a line of the given angle. The direction of each of the child 
-- trees should be computed by adding and subtracting delta from the 
-- parent's angle
--
-- Solution from: https://github.com/x-y-z/2011fall/blob/master/
--                       cis552/hw/2/Main.hs
fractal :: Float -> Float -> Float -> Float -> Tree Float -> Tree Picture
fractal _ _ _ _ Leaf = Leaf
fractal d a x y (Branch s t1 t2)
    = Branch (Line [(x,y),(x',y')]) (fractal d (a+d) x' y' t1)
                                    (fractal d (a-d) x' y' t2)
    where x' = s * (cos a) + x
          y' = s * (sin a) + y
          
t1b :: Test
t1b = "1b" ~: fractal (pi/2) 0 0 0 (calcSize 0.5 25) ~=? 
               Branch (Line [(0.0,0.0),(25.0,0.0)]) 
                  (Branch (Line [(25.0,0.0),(25.0,12.5)]) Leaf Leaf) 
                  (Branch (Line [(25.0,0.0),(25.0,-12.5)]) Leaf Leaf)

-- 1 (c) 
-- convert the tree into a single picture
-- Solution from: https://github.com/x-y-z/2011fall/blob/master/
--                       cis552/hw/2/Main.hs
join :: Tree Picture -> Picture
join = foldTree Blank (\a t1 t2 -> pictures ([a] ++ [t1] ++ [t2]))

t1c :: Test
t1c = "1c" ~: join (Branch Blank Leaf Leaf) 
           ~?= Pictures [Blank, Blank, Blank]

-- | create a fractal tree with some initial parameters. Try changing 
-- some of these values to see how that changes the tree. In particular, 
-- try changing the delta for the angle of the branches 
-- (initially pi/6 below).
-- [Note: changing the angle narrows or widens the tree]
fractalTree :: Picture
fractalTree = color blue (join (fractal (pi/6) (pi/2) 0 (-200) sizeTree)) where
   sizeTree = calcSize 0.6 150.0

drawTree :: IO ()
drawTree = displayInWindow "MyWindow" (400,400) (10,10) white fractalTree

test1 :: Test
test1 = TestList [t1a,t1b,t1c]

----------------------------------------------------------------------
{- 3
    Transform a  SimpleXML document from one form (a PLAY) to
    another (HTML).
    
    The purpose of this assignment is not just to “get the job done”—i.e., to produce the right HTML. A more important goal is to think about what is a good way to do this job, and jobs like it.

    To this end, your solution should be organized into two parts:

     1. a collection of generic functions for transforming XML 
        structures  that have nothing to do with plays, plus

     2. a short piece of code (a single function definition or a 
        collection of short functions) that uses the generic functions 
        to do the particular job of transforming a play into HTML.
       
    Obviously, there are many ways to do the first part. The main challenge of the assignment is to find a clean design that matches the needs of the second part. You will be graded not only on correctness (producing the required output), but also on the elegance of your solution and the clarity and readability of your code and documentation. As always, style most definitely counts.    
    
    Notes:
    =====
    
    Basic mapping of Play to HTML:
    
    0   PLAY                                    html, body
    1   |- TITLE    [PCDATA]                    h1
    1   |- PERSONAE                             h2, "Dramatis Personae"
    2       |- PERSONA* [PCDATA]                br
    1   |- ACT*                                 
    2       |- TITLE    [PCDATA]                h2
    2       |- SCENE*
    3            |- TITLE   [PCDATA]            h3
    3            |- SPEECH*
    4                 |- SPEAKER  [PCDATA]      b, br
    4                 |- LINE*    [PCDATA]      br
                      
    (*) 1 or more           

    HTML as SimpleXML structure:
        html
          |- body
              |- h1  [PCDATA]
              |- h2* [PCDATA]
                  |- h3* [PCDATA]
                      |- b* [PCDATA]    -- b and br not really elements
                      |- br* [PCDATA]   -- every PCDATA followed by a br
                      
    Second Refactoring Notes:
    ========================
           
    1. How to generalize element transformation patterns?
        PLAY
            Element tag children -> 
                [Element newTag  [Element newTag (concatMap children)]]  
        
        TITLE
            Element tag child    -> Element newTag child
            
        PERSONAE 
            Element tag children -> new Data Element ++ concatMap children
            
        PERSONA, LINE 
            Element tag child    -> child ++ new Element
            
        ACT, SCENE 
            Element tag (x:xs)   -> [Element tag x] ++ concatMap children
            
        SPEECH 
            Element tag children -> concatMap children
            
        SPEAKER
            Element tag child   -> [Element tag child] ++ new Element
            
    2. Wrote four generic functions: mapChildren, newElement,
       processChildren, appendChild, based on above transform patterns
       
    3. Tightened up code, reducing solution to one function with
       an internal helper
       
-}
-- generic functions
type Tag = String

mapChildren :: (SimpleXML -> [SimpleXML]) -> [SimpleXML] -> [SimpleXML]
-- | Map the given function to the given child elements,
--   concatenating the results.
mapChildren f = concatMap f

newElement :: Tag -> [SimpleXML] -> SimpleXML
-- | Create a new element with the given tag and children
newElement tag cs = Element tag cs

processChildren :: Tag -> (SimpleXML -> [SimpleXML]) ->[SimpleXML] 
                       -> [SimpleXML]
-- | Create a new element from the given tag and first child,
--   apply the given function to the remaining children
processChildren tag f ((Element _ d):cs) = 
    [Element tag d] ++ mapChildren f cs
processChildren _ _ xml = xml    

appendChild :: SimpleXML -> [SimpleXML] -> [SimpleXML]
-- | Append the given element to the given child elements
appendChild c cs = cs ++ [c]

-- 

formatPlay :: SimpleXML -> SimpleXML
formatPlay (Element "PLAY" children) =
    Element "html" [Element "body" (mapChildren fmt children)]
    where
    fmt :: SimpleXML -> [SimpleXML]
    fmt (Element "TITLE"    c)  = [newElement "h1" c]
    fmt (Element "PERSONAE" cs) = dm : mapChildren fmt cs
    fmt (Element "PERSONA"  c)  = appendChild br c 
    fmt (Element "ACT"      cs) = processChildren "h2" fmt cs
    fmt (Element "SCENE"    cs) = processChildren "h3" fmt cs
    fmt (Element "SPEECH"   cs) = mapChildren fmt cs
    fmt (Element "SPEAKER"  c)  = appendChild br [newElement "b" c]
    fmt (Element "LINE"     c)  = appendChild br c
    fmt _ = error "formatPlay: invalid child tag"
    
    -- define additional required HTML elements
    dm, br :: SimpleXML
    dm = newElement "h2" [PCDATA "Dramatis Personae"]
    br = newElement "br" []

formatPlay _ =  error "formatPlay: not in PLAY format"        

-- provided test code
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ take 20 cs ++ 
            "' vs '" ++ take 20 ds

test3 :: Test
test3 = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

runTest3 :: IO Counts
runTest3 = runTestTT test3
