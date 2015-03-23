module TFWH08E where

{-
    Chapter 8 - Pretty Printing - Notes and Exercises
    
        Creating a library of functions for the pretty
        printing of text
    
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/

-}
type Layout = String
type Doc    = [Layout]

-- simple document definitions
nil, line :: Doc
nil  = [""]         -- empty document
line = ["\n"]       -- document with a single empty line

-- convert a string to a document
text :: String -> Doc
text s = [s]

-- concatenate two documents
(<>) :: Doc -> Doc -> Doc
xss <> yss = [ xs ++ ys | xs <- xss, ys <- yss ]

-- return all possible layouts for the given document
layouts :: Doc -> [Layout]
layouts = id

-- nest a document using the given indentation
nest :: Int -> Doc -> Doc
nest i = map (nestl i)
    where
        nestl i    = concat . map (indent i)
        indent i c = if c == '\n'
                     then c : replicate i ' '
                     else [c]

-- add an extra layout with a single line and no line breaks
group :: Doc -> Doc
group x = flatten x ++ x

-- combine document layouts into a single layout, replacing each
-- newline and its indentation with a single space
flatten :: Doc -> Doc
flatten x = [flattenl (head x)]

flattenl :: Layout -> Layout
flattenl [] = []
flattenl (c:cs) | c == '\n' = ' ' : flattenl (dropWhile (== ' ') cs)
                | otherwise = c : flattenl cs
                
-- return the shape of a layout (a list of the length of the lines
-- that make up the layout)               
shape :: Layout -> [Int]               
shape = map length . lines 

-- Examples

-- -----------------------------------------------------------------------
-- 1. Laying out conditional expressions 
-- -----------------------------------------------------------------------

data CExpr = Expr String | If String CExpr CExpr

-- function expresses acceptable layouts for a conditional expression
cexpr :: CExpr -> Doc
cexpr (Expr p)   = text p
cexpr (If p x y) = group 
                  (group (text "if " <> text p <>
                          line <> text "then " <>
                          nest 5 (cexpr x)) <>
                   line <> text "else " <>
                   nest 5 (cexpr x))

-- example expressions                   
ce1, ce2, ce3 :: CExpr
ce1 = If "happy" (Expr "joyous") (Expr "miserable")
ce2 = If "happy" (If "wealthy" (Expr "fantastic") (Expr "pleasant")) 
                 (If "in love" (Expr "idyllic") (Expr "miserable"))
ce3 = If "wealthy" (If "happy" (Expr "lucky you") (Expr "tough"))
                   (If "in love" (Expr "content") (Expr "miserable"))
     
ex1a = cexpr ce1              -- returns 3 possible layouts
ex1b = map shape ex1a         -- [[32],[20,11],[8,11,11]]

ex1c  = map shape (cexpr ce2)  -- returns 13 possible layouts
ex1d  = map shape (cexpr ce3)  -- returns 13 possible layouts
                            
-- -----------------------------------------------------------------------                            
-- 2. Laying out general trees
-- -----------------------------------------------------------------------
data GenTree a = Node a [GenTree a]

gtree :: Show a => GenTree a -> Doc
gtree (Node x []) = text ("Node " ++ show x ++ " []")
gtree (Node x ts) = text ("Node " ++ show x) 
                 <> group (nest 2 (line <> bracket ts)) -- subexpressions
                 
bracket :: Show a => [GenTree a] -> Doc
bracket ts = text "[" <> nest 1 (gtrees ts) <> text "]"
    where
        gtrees [t] = gtree t
        gtrees (t:ts) = gtree t <> text ", " <> line <> gtrees ts
        
-- example trees     
gt1 = Node 2 [Node 7 [],Node 8 []]   
gt2 = Node 1 [Node 2 [Node 7 [],Node 8 []]]
gt3 = Node 1 [Node 2 [Node 7 [],Node 8 []],
              Node 3 [Node 9 [Node 10 [],Node 11 []]],
              Node 4 [],
              Node 5 [Node 6 []]]
        
ex2a = gtree gt1                -- returns 2 possible layouts
ex2b = map shape ex2a           -- [[30],[6,14,13]]

ex2c = map shape (gtree gt2)    -- returns 3 possible layouts
ex2d = map shape (gtree gt3)    -- returns 13 possible layouts

-- -----------------------------------------------------------------------
-- 3. Laying out paragraphs
-- -----------------------------------------------------------------------

para :: String -> Doc
para = cvt . map text . words
    where
    cvt []     = nil
    cvt (x:xs) = x <> foldr (<>) nil [group (line <> x) | x <- xs]
        
-- example paragraphs
pg1 = "This is a paragraph of a number of lines. Each line is going to be "
      ++ "pretty printed."
pg2 = "This is a short paragraph with a number of lines of fairly short \
       \ words. " ++
      "Each line is going to be pretty printed within a total line width \
      \ of " ++ 
      "length w. The process will be greedy: if the next word fits it \
      \ will " ++
      "be placed on the current line. Otherwise a new line is started."

-- this actually takes much longer than 31.32 secs      
pg3 = "This is a fairly short paragraph with just twenty-two words. \
       \ The problem is that pretty-printing it takes time, in fact 31.32 \
       \ seconds."
pg4 = "A lost and lonely hippopotamus went into a bar."
        
ex3a = para pg1     -- gives an infinite length document

-- return the best layout for the given line width and document
pretty :: Int -> Doc -> Layout
pretty w = fst . foldr1 choose . map augment
    where
        augment lx           = (lx, shape lx)
        choose alx aly       = if better (snd alx) (snd aly)
                               then alx else aly
                          
        better [] ks         = True
        better js []         = False
        better (j:js) (k:ks) | j == k = better js ks
                             | otherwise = (j <= w)

ex3b = putStrLn $ pretty 30 $ para pg4      -- 0.00 secs
ex3c = putStrLn $ pretty 30 $ para pg1      -- 1.70 secs

-- paragraphs 2 and 3 take way too long

                            
                 