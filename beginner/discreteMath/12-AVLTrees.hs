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
st1 = Nub    -- height: 0

{-
    st2                     5120  
    height: 2               /  \
                          1143 9605
                          /  \  / \
                          N  N  N  N
-}
st2 = Cel 5120 "PDA Cam"  (Cel 1143 "Ink Jet"    Nub Nub)
                          (Cel 9605 "Palm Pilot" Nub Nub)
                          
{-
    st3                         5120
    height: 3                   /  \
                             1143  9605
                             /  \  /   \
                            N   N 9600  N
                                   / \
                                   N  N
-}                          
st3 = Cel 5120 "PDA Cam"  (Cel 1143 "Ink Jet"    Nub Nub)
                          (Cel 9605 "Palm Pilot" 
                             (Cel 9600 "Laptop" Nub Nub)
                             Nub)
{- st 4 unordered tree
                                    5120
                                    /  \
                                  9603 1143
                                  /  \  / \
                                  N  N  N  N
-}
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
    
{-
    12.6 Balanced Trees
    
    A 'node balanced' tree has the same number of nodes on the
    both the left and right sides of the tree; insertion under
    node balancing is expensive.
    
    A tree is 'height balanced' if, at all levels, the height of the
    left and right subtrees are within a difference of one.
        
    Height-balanced trees have the property that height
    grows at the same rate as the logarithm of the number of data items. 
    This makes it possible to carry out retrieval, insertion, and
    deletion efficiently.
    
    Any insertion method should satisfy the following properties:
    
    1. The new key is inserted according to key ordering.
    2. The tree remains balanced after the insertion.
    3. No nodes are deleted during the insertion.
    4. The new key exists in the tree after insertion.
    5. Only the new key is added i.e. only one key added at a time
    
    Re-balancing trees, after an insertion, gets tricky as the tree
    grows. There are 4 special cases that occur as part of the
    general problem; two are 'easy', the other two require an
    'ingenious insight'.
    
    Re-balancing the Easy Cases
    ---------------------------
    If we start with a tree that is ordered and balanced and insert
    a new key so that key order is preserved, the worst that can
    happen is that the height of the subtree with the insertion
    is 2 greater than the height of the other subtree.
    
    Left subtree insertion rebalanced with Easy Right Rotation
    ----------------------------------------------------------
        The right subtree is an ordered, balanced tree of height
        'n' and the left subtree is an ordered, balanced tree of
        height n+2
        
        We can also assume that this left-subtree's own left
        subtree is ordered and balanced tree of height n+2 and
        a right subtree of of n or n+1
          
              Height          Height        Keys:
               n+3       z     n+1            xL keys < x
                |       / \     |             x < xR keys < z
               n+2     x  zR    |             z < zR keys
                |     / \
               n+1   xL xR  (Note: xR can be n or n+1 in height)
                
        Formula:  Cel z d (Cel x a xL xR) zR
                
        The above tree is 'outside left heavy'; tallest part is
        the left subtree of the left side.  
        
        This tree can be re-balanced using 'easy right rotation'
        to give us the following tree:
        
             Height            Height       Keys:
               n+2       x      n+3           xL keys < x
                |       / \      |            x < xR keys < z
               n+1     xL  z     |            z < zR
                |         / \    |
                n        xR zR   | (xR has a height of n or n+1, zR n)
                
        Formula: Cel x a xL (Cel z d xR zR)
                         
        Visually, we've rotated 'x' to the right, placing it in
        the root position; our previous root, 'z', becomes
        the right subtree of 'x' and 'xR', the old right subtree of
        'x' becomes the left subtree of 'z'. 
        
        The key order of each individual subtree is maintained and
        balance is restored. (Previously the left-side was n+3
        and the right, n+1, for a difference of 2, now the left
        side is n+2 and the right-side n+3 for a difference of 1).
              
-}
{- 
    st5                       30
    height: 4                /  \
    balanced: False        20    40
    ordered: True         /  \   / \        'outside left heavy'
                         10  25  N  N
                        /  \ / \
                       9   N N N
                      / \
                      N  N
                        
-}
st5 = (Cel 30 "30" (Cel 20 "20"
                       (Cel 10 "10" 
                          (Cel 9 "9" Nub Nub)
                          Nub)
                       (Cel 25 "25" Nub Nub))
                    (Cel 40 "40" Nub Nub))
                    
-- determine whether or not a tree is height balanced
balanced :: SearchTree d -> Bool
balanced (Cel k d lt rt) = (abs (height lt - height rt)) <= 1
balanced _               = False

-- determine whether or not a tree is 'outside left heavy'
isOLH :: SearchTree d -> Bool
isOLH (Cel z d (Cel x a xL xR) zR) =
    let hxl = height xL
        hxr = height xR
        hzr = height zR
    in   (hxl >= hxr) && (hxl <= hxr + 1) 
       &&(hxr >= hzr) && (hxl == hzr + 1)
isOLH Nub = False       
       
-- balances a tree that is outside left heavy       
easyRight :: SearchTree d -> SearchTree d
easyRight tree@(Cel z d (Cel x a xL xR) zR) =
    (Cel x a xL (Cel z d xR zR))
easyRight tree   = tree

{-
    st6 - result of 'easyRight st5'
    height: 3
    balanced: True          20
    ordered: True         /    \
                         10    30
                        /  \  /  \
                       9   N 25   40
                      / \   /  \  / \
                      N N   N  N  N  N
                        
                         
-}
st6 = Cel 20 "20" (Cel 10 "10" 
                      (Cel 9 "9" Nub Nub) Nub) 
                  (Cel 30 "30" 
                      (Cel 25 "25" Nub Nub) 
                      (Cel 40 "40" Nub Nub))

{-        
    Right subtree insertion rebalanced with Easy Left Rotation
    ----------------------------------------------------------
    Mirror image of what happened above; the tree is unbalanced
    being 'outer right heavy' and can be balanced using
    'easy left rotation'
-}    
{-
    st7
    height: 4
    balanced: False         20
    ordered: True         /    \
                         10    30
                        /  \  /  \        'outer right heavy'
                       N   N 25   40
                            /  \  / \
                            N  N  N 50

-}
-- create an 'outer right heavy' tree
st7 = Cel 20 "20" (Cel 10 "10" Nub Nub) 
                  (Cel 30 "30" 
                      (Cel 25 "25" Nub Nub) 
                      (Cel 40 "40" 
                            Nub
                           (Cel 50 "50" Nub Nub)))
            
-- determine whether or not a tree is 'outer right heavy'            
isORH :: SearchTree d -> Bool
isORH (Cel z d zL (Cel y b yL yR)) = 
    let hyl = height yL
        hyr = height yR
        hzl = height zL
    in     hyr >= hyl && hyr <= hyl + 1
        && hyl >= hzl && hyr == hzl + 1
isORH _ = False

-- re-balance an 'outer right heavy' tree
easyLeft :: SearchTree d -> SearchTree d
easyLeft tree@(Cel z d zL (Cel y b yL yR)) =
    (Cel y b (Cel z d zL yL) yR)
easyLeft tree    = tree

{-
    st8 - result of 'easyLeft st7'
    height: 3
    balanced: True          30
    ordered: True         /    \
                         20    40
                        /  \  /  \     
                      10   25 N  50
                     /  \ / \    / \
                    N   N N  N   N N
    

-}
st8 = Cel 30 "30" 
        (Cel 20 "20" 
            (Cel 10 "10" Nub Nub) 
            (Cel 25 "25" Nub Nub)) 
        (Cel 40 "40" Nub (Cel 50 "50" Nub Nub))

{-
    Re-balancing the hard cases
    ---------------------------
    The easy cases occur when the imbalances are on the outer part
    of the tree; the hard cases occur when the imbalances occur
    on the inner trees.
    
    The 'inside right heavy case' Happens when:
        the right subtree is 2 more than the left subtree, and
        that right subtree's left tree is the tallest
        
    The fix is to apply an 'easyRight' followed by an 'easyLeft'
        
        
        z                       z                        x
      /   \     easyRight     /    \     easyLeft      /   \
     zL    y      --->       zL     x      --->       z     y
          /  \                    /   \             /   \  /  \
         x   yR                 xL     y          zL    xL xR yR
        / \                           / \
       xL xR                         xR yR
       
           IRH                     Unbalanced         Balanced
        
    Formula:
        IRH  --> Cel z d zL (Cel y b (Cel x a xL xR) yR)
        Ubal --> Cel z d zL (Cel x a xL (Cel y b xR yR))
        Bal  --> Cel x a (Cel z d zL xL) (Cel y b xR yR)
        
    An unbalanced tree with 'inside left heavy' case is corrected
    by an easyLeft followed by an easyRight
-}
{-
    st9
    height:   4
    balanced: False         20
    ordered:  True        /    \
                         10    30
                        /  \  /  \        'inside right heavy'
                       N   N 25   40
                            /  \  / \
                           23  27 N  N
                          /  \/ \
                          N  NN N

-}
st9 = Cel 20 "20" (Cel 10 "10" Nub Nub) 
                  (Cel 30 "30" 
                      (Cel 25 "25" 
                          (Cel 23 "23" Nub Nub)
                          (Cel 27 "27" Nub Nub))
                      (Cel 40 "40" Nub Nub))
       
-- determine if a tree is 'inside right heay'
isIRH :: SearchTree d -> Bool
isIRH (Cel z d zL zR@(Cel y b yL yR)) = 
    height zL < height zR && height yL > height yR
isIRH _ = False

-- balances a tree that is left-heavy (inside or out)
rotR (Cel z d (Cel x a xL xR) zR) =
    if (height xL) < (height xR)
    then easyRight (Cel z d (easyLeft (Cel x a xL xR)) zR)
    else easyRight (Cel z d (Cel x a xL xR) zR)

-- balances a tree that is right-heavy (inside or out)
rotL (Cel z d zL (Cel y b yL yR)) =
    if (height yR) < (height yL)
    then easyLeft (Cel z d zL (easyRight (Cel y b yL yR)))
    else easyLeft (Cel z d zL (Cel y b yL yR))
   
{-
    st10 - result of 'rotL st9'
    height:   3
    balanced: True          25
    ordered:  True        /     \
                         20      30
                        /  \    /  \        
                      10   23  27   40
                     /  \ / \ / \  / \
                     N  N N N N N  N N

-}   
st10 = Cel 25 "25" (Cel 20 "20" 
                       (Cel 10 "10" Nub Nub) 
                       (Cel 23 "23" Nub Nub)) 
                   (Cel 30 "30" 
                       (Cel 27 "27" Nub Nub) 
                       (Cel 40 "40" Nub Nub))
    
{-
    st12
    height:   4
    balanced: False                 20
    ordered:  True                /    \
                                10     30     'inside left heavy'
                               /  \   /   \
                              8   12  N    N
                             / \ /  \
                             N N 11  15
                                /  \ / \
                                N  N N N
-}   
st12 = Cel 20 "20" (Cel 10 "10"
                       (Cel 8 "8" Nub Nub)
                       (Cel 12 "12"
                            (Cel 11 "11" Nub Nub)
                            (Cel 15 "15" Nub Nub)))
                   (Cel 30 "30" Nub Nub) 
 
{-
    st13 - result of 'rotR st12'
    height:   3
    balanced: True          12
    ordered:  True        /     \
                         10      20
                        /  \    /  \        
                       8   11  15   30
                     /  \ / \ / \  / \
                     N  N N N N  N  N N

-}    
st13 = Cel 12 "12" (Cel 10 "10" 
                       (Cel 8 "8" Nub Nub)  
                       (Cel 11 "11" Nub Nub)) 
                   (Cel 20 "20" 
                       (Cel 15 "15" Nub Nub) 
                       (Cel 30 "30" Nub Nub))
                   
{-
    When inserting a new item we need to decide which subtree
    it goes into; an inductive process as we usually need to
    traverse the tree to find an empty spot. Once we've made
    the insertion, we need to check the height of the insertion
    tree compared to the height of the tree on the other side,
    if it is 2 or more higher, we need to invoke a rotation.
-}
{- insert new node  
insert key obj  Nub = (Cel key obj Nub Nub)  -- new tree
insert key obj tree@(Cel eKey eObj xt yt)
    | xt == Nub && yt == Nub = insert' key obj key tree
    | xt == Nub              = insert' key obj (getZ yt) tree
    | otherwise              = insert' key obj (getZ xt) tree
    where
    getZ (Cel z _ _ _) = z
    
    insert' k d z (Cel x a lt rt) =
        if z < x then
                 if (height newL) > (height rt) + 1
                    then rotR (Cel k d newL rt)
                    else (Cel x a newL rt)
        else if z > x then
            if (height newR) > (height lt) + 1
               then rotL (Cel x a lt newR)
               else (Cel x a lt newR)
        else (Cel x d lt rt)
        where
            newL = insert k d lt
            newR = insert k d rt    
-}

insert k d Nub = (Cel k d Nub Nub)
insert k d(Cel x a lf rt) = 
    if k < x then
        if (height newL) > (height rt) + 1
        then rotR (Cel k d newL rt)
        else (Cel x a newL rt)
    else if k > x then
        if (height newR) > (height lf) + 1
        then rotL (Cel x a lf newR)
        else (Cel x a lf newR)
    else (Cel x d lf rt)
    where
        newL = insert k d lf
        newR = insert k d rt            
                
-- try a series of inserts to build a new tree
-- have to start with ordered, balanced tree
-- if you start from a Nub and build up, adding to right subtee
-- doesn't work correctly
newTree = insert 2088 "2088" (Cel 4403 "4403"
                                (Cel 1143 "1143" Nub Nub)
                                (Cel 7268 "7268" Nub Nub))
nt1 = insert 2000 "2000" newTree
nt2 = insert 5000 "5000" nt1

newTreeA = insert 7628 "7628" Nub
nt1a     = insert 1143 "1143" newTreeA
nt2a     = insert 4403 "4403" nt1a      -- loses first node
   -- nt2a = Cel 4403 "4403" (Cel 1143 "1143" Nub Nub) 
   --                        (Cel 4403 "4403" Nub Nub)

-- unbalanced trees should be balanced after an insert
st9i  = insert 12 "12" st9       
st12i = insert 50 "50" st12     

checkOrdAndBal = ordered st9i && balanced st9i
              && ordered st12i && balanced st12i        -- True
              
{-
    Insertion in Logarithmic Time
    -----------------------------
    We want the number of steps required for insertion to be proportional 
    to the tree height.
   
    The 'Nub' constructor takes a fixed amount of time.
   
    For the 'Cel' constructor:
        Assume the tree components have already been constructed
        Haskell refers to constructed entities by [memory] 'address'
        addresses have fixed sizes so construction of a 'Cel' takes
        place in fixed time, regardless of the key, data object,
        or subtree sizes
        
    Rotation operators (rotL, rotR) combine a fixed number of Cel
    and Nub constructions so they complete in a fixed number of steps
    
    The 'height' function:
        every time we use 'height' we traverse the tree, this is
        time consuming; so we need a new 'height' operator.
        
        The trick is to avoid repeatedly calculating the tree
        height; we can do this by storing the height in the tree.
        
    The new definition of our search tree would take an extra
    Integer argument to hold the height of the tree, allowing
    us to write 'height' as a fixed time operation.
    
    All the other tree operations have to be modified to use the
    new 'height' argument.
        
-}      
{-
    Deleting an Item
    ----------------
    When deleting an item we want to be sure the result tree remains
    ordered and balanced.
    
    The firs step is to figure out how to delete a key at the root
    of a tree. We can break this into two cases, one easy, one hard.
    The 'easy' case occurs when the left subtree is a Nub; all we
    have to do is return the right subtree.
    
    When the left subtree is not null we have more to do. The idea
    is to march down 'the spine' on the right side of the left subtree
    until we reach a Cel whose right side is a Nub; we'll call this
    node the 'sacrum'. The 'sacrum' is the node with the key 'x' in
    the leftmost tree. The left subtree of the sacrum will have a
    height of 0. We cache the key and data of the sacrum, replace
    it by its left subtree and perform right rotations as needed
    all the way back up the spine.
    
    When we reach the top of the spine we create a new tree with
    (1) the key and date being the key and data items cached from 
        the sacrum
    (2) the left subtree being the one we constructed as we came
        back up the spine
    (3) the right subtree being the right subtree from the original
        tree
        
    Last, if necessary, we rotate the tree left to get back in balance.
    
    Placing the last, sacrum, key and data at the root preserves order
    as it was the largest key in the left subtree and smaller than
    the root and right subtree keys.
    
    To write the above as formulae it helps to consider the various
    parts of the transformation separately.
    
    Shrinking the Spine
    -------------------
    Part of the transformation involves creating a new left subtree
    with a shrunken spine and the sacrum. 
-}

-- shrink the spine of the tree
shrink :: SearchTree d 
       -> (Integer,             -- key from sacrum
           d,                   -- data object from the sacrum
           SearchTree d)        -- new left subtree
shrink(Cel y b lf rt) = 
    if rt == Nub then (y, b, lf)
    else if (height lf) > (height shrunken) + 1
    then (x, a, rotR (Cel y b lf shrunken))
    else (x, a, Cel y b lf shrunken)
    where
        (x, a, shrunken) = shrink rt
        
-- function to delete the root of a tree
delRoot :: SearchTree d -> SearchTree d
delRoot (Cel z d Nub zR) = zR 
delRoot (Cel z d (Cel y b lf rt) zR) = 
    if (height zR) > (height shrunken) + 1
    then rotL (Cel x a shrunken rt)
    else (Cel x a shrunken rt)
    where
        (x, a, shrunken) = shrink (Cel y b lf rt)        
        
{-
    With the above functions in place we can now look at what's
    needed to delete a key; any key to be deleted will be at
    the root of a subtree of the original tree. First, we need
    to locate that subtree and call 'delRoot' and then balance
    the tree, if necessary.
    
-}  
delete Nub k = Nub
delete tree@(Cel z d lf rt) k 
    | k == z   = delRoot tree
    | k < z    = if (height rt) > (height newL) + 1 
                 then rotL (Cel z d newL rt)
                 else (Cel z d newL rt)
    |otherwise = if (height lf) > (height newR) + 1
                 then rotR (Cel z d lf newR)
                 else (Cel z d lf newR)
    where
        newL = delete lf k
        newR = delete rt k      
        
st14 = delete st13 11       -- remains ordered and balanced
st15 = delete st13 20       -- remains ordered and balanced
st16 = delete st13 12       -- removing the root at the top leaves
                            -- the tree unordered and with a duplicate
                            -- key
{- 
    Cel 11 "11" (Cel 10 "10" 
                  (Cel 8 "8" Nub Nub) 
                   Nub) 
                (Cel 11 "11" Nub Nub)      
-}                          
