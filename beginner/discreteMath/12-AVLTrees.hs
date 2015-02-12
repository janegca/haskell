-- Chapter 12 - AVL Trees
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

{-  
    Finding a folder (Binary Search)
    --------------------------------
    Looking for a folder in a filing cabinet where folders are
    kept in numerical order; keep dividing search area in half
    until you reach the required folder  i.e. 100 50 25 12 6 3 1  
    worst case, 6 steps to find your folder. This is compared
    to a random method of searching which, on average, will require
    you to look at 50 folders (100/2)
    
    So, if you have 'n' folders, the most steps you have to take
    is equal to the number of successive halvings you need to make
    to get down to 1 folder. That number is the base 2 logarithm
    of n.  
    
    New File Problem
    ----------------
    The above works well until you hit the 'new file problem' 
    where fitting a new file into the cabinet requires a
    re-arrangement of multiple cabinets. The same happens when
    a folder needs to be deleted; you may need to do a lot
    of re-arranging. On average, the re-arranging consumes as
    much time as a random search: n/2 so you lose the log_2 n
    efficiency.
    
    The AVL Tree
    ------------
    In the '60's, two Russian mathematicians, Adelson-Velski and
    Landis figured out a way to store folders in a tree structure
    so that all operations take, at most, log(n) steps; the structure
    they invented is known as an AVL Tree; the simplest of which
    is a tree that makes a binary search easy.
    
    Each 'node' in the tree (except leaf nodes) has a folder, a
    left subtree and a right subtree. All the folders in the
    left subtree have numbers less-than the node folder; all 
    the folders in the right subtree have folders larger than
    than the node folder. To find a folder, start with the
    root and look to the left or right depending on the number
    of the folder being sought; repeat until you reach the required
    folder. At worst, the number of steps taken will equal the
    number of levels (height) in the tree. If the tree is balanced, 
    the height will be approx. log_2 n.
    
    The hard part in all this is keeping the tree balanced when 
    making insertions and deletions.
    
    Search Trees and Occurrence Keys
    --------------------------------
    
    We can create a data type for the tree:
    
        data SearchTree d = Nub |
                            Cel Integer d (SearchTree d) (SearchTree d)
        where,
            Nub is a Leaf (empty node)
            Cel is a node with left and right subtrees
            Intger is the 'key'
            'd' is the 'type' of the data object
            
    Example Tree:
        Cel 5120 "PDA Cam"  (Cel 1143 "Ink Jet" Nub Nub)
                            (Cel 9605 "Palm Pilot" Nub Nub)
                            
                                5120 PDA Cam
                                    /    \
                          1143 Ink Jet  9605 Palm Pilot
                               /    \       /     \
                              Nub   Nub    Nub    Nub
        
        the 'keys' are numbers, the data objects, Strings
        
    We can define properties over the tree
                         
-}
module AVL where

data SearchTree d = Nub 
                  | Cel Integer d (SearchTree d) (SearchTree d)
    deriving Show

-- example trees    
st1 = Nub    
st2 = Cel 5120 "PDA Cam"  (Cel 1143 "Ink Jet"    Nub Nub)
                          (Cel 9605 "Palm Pilot" Nub Nub)
                          
st3 = Cel 5120 "PDA Cam"  (Cel 1143 "Ink Jet"    Nub Nub)
                          (Cel 9605 "Palm Pilot" 
                             (Cel 9600 "Laptop" Nub Nub)
                             Nub)
-- unordered tree
st4 = Cel 5120 "PDA Cam"  (Cel 9605 "Ink Jet"    Nub Nub)
                          (Cel 1143 "Palm Pilot" Nub Nub)
                             
-- compares two SearchTrees for equality                    
instance Eq (SearchTree d) where
    Nub == Nub               = True             -- {N == N}
    Nub == (Cel k d lt rt)   = False            -- {N == C}
    (Cel k d lt rt) == Nub   = False            -- {C == N}
    (Cel x a xl xr) == (Cel y b yl yr) =        -- {C == C}
        (x == y) && (xl == yl) && (xr == yr)
           
-- returns True if the first tree is a subset of the second          
subtree :: SearchTree d -> SearchTree d -> Bool
subtree Nub s               = True                 -- {N subset}
subtree (Cel k d lt rt) Nub = False                -- {C subset N}
subtree lt@(Cel x a xl xr) rt@(Cel y b yl yr) =    -- {C subset C}
    (lt == rt) || subtree lt yl || subtree lt yr
    
-- returns True if the first tree is a proper subset of the second
propSubtree :: SearchTree d -> SearchTree d -> Bool
propSubtree s Nub             = False
propSubtree s (Cel k d lt rt) = (subtree s lt) || (subtree s rt)
   
-- returns True if the given key is in the tree
hasKey :: Integer -> SearchTree d -> Bool
hasKey k Nub              = False
hasKey k (Cel k' d lt rt) = (k == k') || hasKey k lt || hasKey k rt

{-
    Ordered Search Trees
    --------------------
    A search tree is ordered if the key in each non-leaf node
    is greater than all the keys in its left subtree and less
    than all the keys in its right subtree and there are no
    duplicated keys.
    
    
-}
ordered :: SearchTree d -> Bool
ordered Nub = True
ordered (Cel k d lt rt) | compLKey lt == False = False
                        | compRKey rt == False = False
                        | otherwise = ordered lt && ordered rt
    where
        compLKey, compRKey :: SearchTree d -> Bool
        compLKey (Cel k' _ _ _) = k' < k
        compLKey Nub            = True
        
        compRKey (Cel k' _ _ _) = k' > k
        compRKey Nub            = True
        
{-
    Tree Induction
    --------------
    Each key should appear once and represent a unique data object
    i.e. no key should appear twice, 
         no data object should have two distinct keys
         
    Using the principal of tree induction
    
    (forall t.((forall s subset t).P(s)) -> P(t)) -> (forall t.P(t))
    
    we can prove that a predicate is true for every search tree if
    we can prove that: if a predicate is true for every proper
    subtree of any particular arbitrarily chosen tree, then it is also
    true for the chosen tree.
        
-}        
-- returns the data object for the given key  
-- key should only appear once, so only one object should be returned      
dataElems :: SearchTree d -> Integer -> [d]
dataElems Nub x             = []                     -- {dataElems N}
dataElems (Cel k d lf rt) x                          -- {dataElems C} 
    | k == x    = (dataElems lf x) ++ [d] ++ (dataElems rt x)
    | otherwise = (dataElems lf x) ++ (dataElems rt x)
  
-- return the data item for the given key or Nothing 
getItem :: SearchTree d -> Integer -> Maybe d
getItem (Cel k d lt rt) x | x < k     = getItem lt x
                          | x > k     = getItem rt x
                          | otherwise = (Just d)
getItem Nub _ = Nothing
  
{-
    Search Time
    ------------
    We can use the 'equational model of computation' to determine
    the search time of our function 'getItem'.
    
    Each 'match' counts as 1 step as does each comparison, so
        if x < k,  1 step for pattern match +
                   1 step for < + # of steps for (getItem lt x)
        if x > k,  1 step for pattern match +
                   1 step for < + 
                   1 step for > + # of steps for (getItem rt x)
        if x == k, 1 step for pattern match +
                   1 step for < +
                   1 step for > +
                   1 step for 'otherwise'  (??)
                   1 step to construct (Just d)

        if Tree is empty, 1 step for match
        
    so,
        gSteps (Nub, x) = 1
        gSteps (Cel k d lt rt) =
           max(gSteps (lt, k) + 2, gsteps (rt,k)+3, 5)

    The number of stages should not exceed the height of the tree
    The height of a tree is the longest path from the root to a Nub,
    so the height is 1 + the taller of the left or right subtree.
    
    From this we can see that the height of a tree can never be
    negative and the form of a tree with 1 data item is
            (Cel k d Nub Nub)
    with 2 data items
            (Cel k d (Cel x a Nub Nub) (Cel y b Nub Nub))
    
    and any subtree can be modified in the same way, so,
    we can compute the number of data items in a tree of
    height 'n' as: 2^n - 1.
    
    For a compact tree, retrieval time is 4 * (height s + 1)
    (see Theorem 85)
    and for a tree with a million data items:
        4 * (log_2 1,000,000 + 1) = approx 80 steps
        
    This can be improved on with more efficient insertion and
    deletion functions.
        
-}  
height :: SearchTree d -> Integer
height Nub            = 0                   -- {height N}
height(Cel k d lt rt) =
    1 + (max (height lt) (height rt))       -- {height C}
    
    
    