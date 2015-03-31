{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

{-
    Preliminary Info
    ----------------
    
    As the most junior employee at Calculators R Us, Inc., you are tasked
    with organizing the office Spring Break party. As with all party
    organizers, your goal is, of course, to maximize the amount of fun 
    which is had at the party. Since some people enjoy parties more than 
    others, you have estimated the amount of fun which will be had by each
    employee. So simply summing together all these values should indicate
    the amount of fun which will be had at the party in total, right?
    
    . . .well, there’s one small problem. It is a well-known fact that
    anyone whose immediate boss is also at the party will not have any
    fun at all. So if all the company employees are at the party, only the
    CEO will have fun, and everyone else will stand around laughing
    nervously and trying to look natural while looking for their boss out
    of the corner of their eyes.
    
    Your job, then, is to figure out who to invite to the party in order to
    maximize the total amount of fun.    

-}

import Data.Monoid
import Data.Tree
import Data.List (sort)
import Employee

-- some test employees
e1, e2, e3, e4 :: Employee
e1 = Emp "Stan" 9
e2 = Emp "Bob" 3
e3 = Emp "Helen" 5
e4 = Emp "Sue" 8

gl1, gl2 :: GuestList
gl1 = GL [e1, e2] 12
gl2 = GL [e3, e4] 13

{-
    Exercise 1
    
    A function

        glCons :: Employee -> GuestList -> GuestList

    which adds an Employee to the GuestList (updating the cached
    Fun score appropriately). Of course, in general this is impossible:
    the updated fun score should depend on whether the Employee
    being added is already in the list, or if any of their direct 
    subordinates are in the list, and so on. For our purposes, though, you
    may assume that none of these special cases will hold: that is,
    glCons should simply add the new Employee and add their fun
    score without doing any kind of checks.    

-}

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = f}) (GL xs s) = GL (e:xs) (s + f)

ex1a :: GuestList
ex1a = glCons e3 gl1

{-
    Exercise 1b
    
    A Monoid instance for GuestList (How is the Monoid instance    
    supposed to work, you ask? You figure it out!)
    
    Note that this requires creating an “orphan instance” (a type class 
    instance instance C T which is defined in a module which is distinct 
    from both the  modules where C and T are defined), which GHC will warn 
    you about. You can ignore the warning, or add 
        {-# OPTIONS_GHC -fno-warn-orphans #-}
    to the top of your file.
-}

instance Monoid GuestList where
    mempty  = GL [] 0
    mappend (GL xs s1) (GL ys s2) = GL (xs ++ ys) (s1+s2)

ex1b :: GuestList    
ex1b = mappend gl1 gl2    
    
{-
    Exercise 1c
    
    A function 
        moreFun :: GuestList -> GuestList -> GuestList
        
    which takes two GuestLists and returns whichever one of them
    is more fun, i.e. has the higher fun score. (If the scores are equal it
    does not matter which is returned.)    

-}
moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ s1) g2@(GL _ s2) | s1 >= s2  = g1
                                  | otherwise = g2
                                  
ex1c :: GuestList                                  
ex1c = moreFun gl1 gl2

{-
    Exercise 2
    
    The Data.Tree module from the standard Haskell libraries defines
    the type of “rose trees”, where each node stores a data element and
    has any number of children (i.e. a list of subtrees):
        data Tree a = Node {
            rootLabel :: a,       -- label value
            subForest :: [Tree a] -- zero or more child trees
        }

    Strangely, Data.Tree does not define a fold for this type! Rectify the
    situation by implementing
    
        treeFold :: ... -> Tree a -> b

    (See if you can figure out what type(s) should replace the dots in
    the type of treeFold. If you are stuck, look back at the lecture notes
    from Week 7, or infer the proper type(s) from the remainder of this
    assignment.)   

    Notes:
        root of each tree is the manager of employees in subtree
        what do we want? total 'fun' score for each subtree?

-}                
-- solution: 
-- https://github.com/pdswan/cis194/blob/master/hw8/Party.hs
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold fn (Node root subTrees) = fn root (map (treeFold fn) subTrees)

{-
    Exercise 3
    
    Write a function
        nextLevel :: Employee -> [(GuestList, GuestList)]
            -> (GuestList, GuestList)
            
    which takes two arguments. The first is the “boss” of the current 
    subtree (let’s call him Bob). The second argument is a list of the 
    results for each subtree under Bob. Each result is a pair of 
    GuestLists: the first GuestList in the pair is the best possible 
    guest list with the boss of that subtree; the second is the best 
    possible guest list without the boss of that subtree. nextLevel 
    should then compute the overall best guest list that includes Bob, 
    and the overall best guest list that doesn’t include Bob.
-}
-- solution: 
-- https://github.com/jroblak/haskell-learnings/blob/master/
--          upenn/hw8/hw8.hs#L23
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss xs = (maxWithBoss, maxSansBoss)
    where maxSansBoss = mconcat $ map (uncurry moreFun) xs;
          maxWithBoss = glCons boss $ maxSansBoss 
                
{-
    Exercise 4

    Finally, put all of this together to define
        maxFun :: Tree Employee -> GuestList
    
    which takes a company hierarchy as input and outputs a fun-maximizing
    guest list. You can test your function on testCompany, provided in
    Employee.hs.
-}
-- solution: 
-- https://github.com/pdswan/cis194/blob/master/hw8/Party.hs

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry max) . (treeFold nextLevel) 
 
ex4, ex5 :: GuestList 
ex4 = maxFun testCompany
ex5 = maxFun testCompany2
 
{-
    Exercise 5

    Implement main :: IO () so that it reads your company’s hierarchy
    from the file company.txt, and then prints out a formatted guest
    list, sorted by first name, which looks like
    
        Total fun: 23924
        Adam Debergues
        Adeline Anselme
        ...
    
    (Note: the above is just an example of the format; it is not the correct
    output!) You will probably find the readFile and putStrLn functions
    useful.
    
    As much as possible, try to separate out the “pure” computation
    from the IO computation. In other words, your main function should
    actually be fairly short, calling out to helper functions (whose types
    do not involve IO) to do most of the work. If you find IO “infecting”
    all your function types, you are Doing It Wrong.
    
-}
-- solution:
-- https://github.com/jroblak/haskell-learnings/blob/master/
--          upenn/hw8/hw8.hs#L23

formatList :: GuestList -> String
formatList (GL emps fun) = "Total fun: " ++ show fun ++ "\n" 
                        ++ (unlines . sort . map empName $ emps)

main :: IO ()
main = do
        file <- readFile "company.txt"
        putStr . formatList . maxFun . read $ file

