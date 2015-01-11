-- Chapter 5 - Trees
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
module Tree05 where

-- a binary tree with an integer value attached to each node
data BinTreeInt = Leaf
                | Node Integer BinTreeInt BinTreeInt
                
tree1 :: BinTreeInt
tree1 = Leaf

tree2 :: BinTreeInt
tree2 = Node 23 Leaf Leaf

{-
    More complex tree
    
                            4
                          /   \
                         2     7
                        / \   /  \
                       1   3  5   8
                       |   | | \  | |
                       L   L L  6 L L
                               / \
                               L L
-}
tree3 :: BinTreeInt
tree3 = Node 4
          (Node 2
            (Node 1 Leaf Leaf)
            (Node 3 Leaf Leaf))
          (Node 7
            (Node 5
              Leaf
              (Node 6 Leaf Leaf))
            (Node 8 Leaf Leaf))

-- Polymorphic Binary Tree -----------------------------------------------
-- the node can hold any data type BUT all nodes must have the same data
-- type

data BinTree a = BinLeaf
               | BinNode a (BinTree a) (BinTree a)
    deriving Show

-- you have to supply the data type when constructing the tree
tree4 :: BinTree String
tree4 = BinNode "cat" BinLeaf (BinNode "dog" BinLeaf BinLeaf)

tree5 :: BinTree (Integer,Bool)
tree5 = BinNode (23,False)
          BinLeaf
         (BinNode (49,True) BinLeaf BinLeaf)
    
tree6 :: BinTree Int
tree6 = BinNode 4
         (BinNode 2
            (BinNode 1 BinLeaf BinLeaf)
            (BinNode 3 BinLeaf BinLeaf))
         (BinNode 6
            (BinNode 5 BinLeaf BinLeaf)
            (BinNode 7 BinLeaf BinLeaf))

-- 5.2 Processing Trees with Recursion -----------------------------------
{-
    Tree Traversal
    
    Visit each node in a tree
    3 commonly used orders
        Preorder  - visit the root, traverse the left subtree and then
                    traverse the right subtree
        Inorder   - traverse left-subtree, then root, then right-subtree
                    [Note: as 'inorder' returns the node values in sorted
                           order the function is often called 'flatten']
        PostOrder - traverse left-subtree, then right-subtree, then root 

    Example:
    
        *Main> inorder tree6
        [1,2,3,4,5,6,7]

        *Main> preorder tree6
        [4,2,1,3,6,5,7]

        *Main> postorder tree6
        [1,3,2,5,7,6,4]
        
        *Main> inorder BinLeaf
        []

        *Main> preorder BinLeaf
        []

        *Main> postorder BinLeaf
        []
        *Main>         
    
-}            
inorder :: BinTree a -> [a]
inorder BinLeaf           = []
inorder (BinNode x t1 t2) = inorder t1 ++ [x] ++ inorder t2

preorder :: BinTree a -> [a]
preorder BinLeaf           = []
preorder (BinNode x t1 t2) = [x] ++ preorder t1 ++ preorder t2

postorder :: BinTree a -> [a]
postorder BinLeaf           = []
postorder (BinNode x t1 t2) = postorder t1 ++ postorder t2 ++ [x]

-- 5.3.2 Processing Tree Structure ---------------------------------------

-- returns a mirror image of a tree (everything is reversed left to right)
reflect :: BinTree a -> BinTree a
reflect BinLeaf         = BinLeaf
reflect (BinNode n l r) = BinNode n (reflect r) (reflect l)

{-
    The height of a tree is equal to the height of its tallest subtree
    plus one (for the root). An empty tree has a height of zero.
-}
height :: BinTree a -> Integer
height BinLeaf           = 0
height (BinNode x t1 t2) = 1 + max (height t1) (height t2)

{-
    The size of a tree is a count of all its nodes plus one for
    the root node. An empty tree has size zero.
-}
size :: BinTree a -> Integer
size BinLeaf           = 0
size (BinNode x t1 t2) = 1 + size t1 + size t2

{-
    An 'unbalanced' tree has a height equal to its size (tree7); all it's
    nodes tend to be on the right or left
    
    A balanced tree has its nodes evenly distributed along the left and
    right sides

-}
tree7, tree8 :: BinTree Integer
tree7 = BinNode 1
            BinLeaf
            (BinNode 2
                BinLeaf
                (BinNode 3 BinLeaf BinLeaf))

tree8 = BinNode 1
    (BinNode 2 BinLeaf BinLeaf)
    (BinNode 3 BinLeaf BinLeaf)
    
-- returns True if a tree is perfectly balanced      
balanced :: BinTree a -> Bool
balanced BinLeaf           = True
balanced (BinNode x t1 t2) = balanced t1 && balanced t2
                          && (height t1 == height t2)
                          
-- 5.3.3 Evaluating Expression Trees -------------------------------------

{-
    A simple expression language can be represented by the 'Exp'
    data type and a corresponding language interpreter can be 
    written as a tree traversal 'eval'
-}
data Exp = Const Integer
         | Add   Exp Exp
         | Mult  Exp Exp
         
eval :: Exp -> Integer
eval (Const n)    = n
eval (Add  e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2         

-- 5.3.4 Binary Search Trees ---------------------------------------------
{-
    Assume a large set of (key, value) pairs and a database of type
    [(a,b)].  One way to find a particular key in the database
    would be to perform a 'linear search' such as 'linSearch'.
    
    The time required for such a search is proportional to the size
    of the database.

-}
linSearch :: Eq a => a -> [(a,b)] -> Maybe b
linSearch k [] = Nothing
linSearch k ((x,y):xs) = if k==x
                         then Just y
                         else linSearch k xs                          
                   
{-
    A more efficient method is to store the data in a 'binary search
    tree' with the caveat that for any key 'k', all the key-value
    pairs in the left subtree will be less than 'k' while all the
    key-value pairs in the right subtree will be greater than 'k'.
    
    As keys are being compared, the key type must be in the ordering 
    (Ord) class.
    
    'bstSearch' searches the left subtree if the current key is less
    than the required key or the right subtree if the current key
    is greater than the required key.  If no key-value pair is found,
    Nothing is returned.
    
    The average time required is proportional to the height of the tree
    approx (log n) vs n/2 for a linear search; the search time improves,
    relative to linear search time, as the database increases.
    
    Eg a db of 1 million records linear search takes 500,00 steps,
       binary search, 20 steps
       if the db increaes to 1 billion records, the binary search
       increases by 50%, to 30 steps, even though the database
       increae was 1000%
-}                   
bstSearch :: Ord a => a -> BinTree (a,b) -> Maybe b
bstSearch key BinLeaf = Nothing
bstSearch key (BinNode (x,y) t1 t2) =
        if key == x
        then Just y
        else if key < x
             then bstSearch key t1
             else bstSearch key t2
                          
{-
    Key to a binary search is the creation and maintenance of a binary
    search tree. The 'insert' method belows builds the database as a well
    balanced binary search tree. Note, this is NOT something the compiler
    can check; the programmer must ensure/prove the algorithm is doing
    what it claims to do. i.e. the compiler can make sure the data
    is properly typed and belongs to Ord but it cannot check the overall
    shape of the tree; an unbalanced binary search tree will be just
    as expensive to search as if one did a linear search.
-}                          
insert :: Ord a => (a,b) -> BinTree (a,b) -> BinTree (a,b)
insert (key,d) BinLeaf = BinNode (key,d) BinLeaf BinLeaf
insert (key,d) (BinNode (x,y) t1 t2) =
    if key == x
    then BinNode (key,d) t1 t2
    else if key < x
         then BinNode (x,y) (insert (key,d) t1) t2 
         else BinNode (x,y) t1 (insert (key,d) t2)
         
-- 5.4.3 Height of a Balanced Tree ---------------------------------------
{-
    If a tree is balanced then
    
        size t = 2^(height t) - 1
            
    Example
        *Main> balanced tree6
        True
        *Main> size tree6
        7
        *Main> height tree6
        3
        *Main> 2^3 - 1
        7
        *Main> 

        *Main> balanced tree7
        False
        *Main> size tree7
        3
        *Main> height tree7
        3
        *Main> 2^3 - 1
        7
              
-}        
-- 5.4.4 Length of a Flattened Tree --------------------------------------
{-
    The length of a flattened tree equals the number of nodes (size)
    of the tree.
    
    Example
        *Main> size tree6
        7
        *Main> inorder tree6
        [1,2,3,4,5,6,7]
        *Main> length it
        7
        *Main> size tree8
        3
        *Main> inorder tree8
        [2,1,3]
        *Main> length it
        3
        *Main>    
-} 
-- 5.5 Execution Time ----------------------------------------------------
{-
    In general, when attempting to determine the execution time of 
    a particular algorithm (function) that traverse a binary tree, 
    we can make the following assumptions (1 unit of time is generic,
    depends on CPU, memory, etc of individual systems):
    
    1. time to execute given a BinLeaf is zero
    2. allocate 1 unit of time for setting up the equation
       i.e. recognizing the pattern, making a recursive call, etc.
    3. determine how many steps are required to complete the operation
       eg (++) xs ys would require (length xs) steps
       
    Assuming we were trying to determine the execution time of
    'inorder' on an arbitrary tree, t
    
    time BinLeaf = 0
    time (inorder (BinNode x t1 t2))
        = 1 + time (inorder t1 ++ [x] ++ inorder t2)
        = 1 + time (inorder t1) + time (inorder t2)
            + length (inorder t1)
        = 1 + time (inorder t1) + time (inorder t2) + size t1
        
    The above is an example of a 'recurrence equation' - an inductive
    equation in which the values being equated are numbers
    
    Here, it is the (size t1) term on the right-hand side of the 
    equation that is the most problematic; it relates to the number
    of nodes we must visit to extract each 'x'. [Remember that 'size'
    is directly related to 'height': size t = 2^(height t) - 1]
    
    If we create recurrence equations for executing 'inorder' over an 
    unbalanced tree (one with ALL right hand subtrees empty) we get the 
    following:
    
        time (inorder (BinNode x t1 BinLeaf))
        = 1 + time (inorder t1) + time (inorder BinLeaf) + size t1
        = 1 + time (inorder t1) + 0 + size t1
        = 1 + time (inorder t1) + size t1
        
        height (BinNode x t1 BinLeaf)
        = 1 + max (height t1) (height t2)
        = 1 + max (height t1) 0
        = 1 + height t1
        
        size (BinNode x t1 BinLeaf)
        = 1 + size t1 + size BinLeaf
        = 1 + size t1    
    
    The height and size equations are identical therefore, the size
    and height of the tree are identical and so the number of recurrence
    steps we need need to reach the empty tree case is equal to 
    to the number of nodes in the tree being flattened. 
    
    [Note: each 'recurrence step' effectively costs 1 time unit, the
           cost of setting up each recurrence; here, a recurrence step
           includes 2 recursive calls, so a recurrence call for a balanced
           tree is:
           
                1 + time recursive call + time recursive call + size t
                
            the '1' time unit is shared (split between) the two 
            recursive calls i.e. each recursive call cost 0.5 time units
                
            While an unbalanced tree, with empty right hand trees is:
            
                1 + time recursive call + 0 + size t
                
            here 'each' recursive call effectively costs 1 full time unit.
    ]
    
    From this, and our knowledge of the time it takes to sum a series
    of numbers (n * (n+1)/2) we can estimate the cost of flattening
    a tree which has only empty right-subtrees as:
    
        time(inorder (BinNode x t1 BinLeaf)) = n(n+1)/2 + n,
            where n = size t1
            
    From which we can see that the number of steps required to flatten
    a tree is proportional to the square of the number of nodes in the
    tree IF all nodes fall on the left-hand side of the tree.
-}

-- 5.6 Flattening trees in Linear Time -----------------------------------
{-
    The 'inorder' function, as it stands, is slow as it repeatedly
    recopies lists as it concatenates them together; we really want
    to build the result directly using a series of partial computations
    that avoid the expensive concatenations.
    
    We want something similar to the following method which usese
    a helper functin, g, that takes an extra list (called a 
    'continuation' [accumulator?]); we can then use the cons 
    operator (:) (which has an almost 0 cost) to build our result list

-}
inorderEfficient :: BinTree a -> [a]
inorderEfficient t = g t []
    where
        g BinLeaf ks           = ks
        g (BinNode x t1 t2) ks = g t1 (x : g t2 ks)

-- check it computes the same result as 'inorder'
testInOrdEff = inorderEfficient tree6 == inorder tree6

{-
    The time of the new method is equal to the size of the tree
    
        time (g (BinNode x t1 t2) ks)
        = time (g t1 (x : g t2 ks))
        = time (g t1 []) + 1 + time (g t2 [])
        = size t1 + 1 + size t2
        = size (BinNode x t1 t2)    

-}
