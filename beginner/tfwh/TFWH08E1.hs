module TFWH08E where

{-
    Chapter 8 - Pretty Printing - Notes and Exercises
    
        Creating a library of functions for the pretty
        printing of text - 2nd attempt
    
    Ref: "Thinking Functionally With Haskell", Richard Bird
          http://www.cs.ox.ac.uk/publications/books/functional/
          
    Following changes have been made to TFWH08E.hs
    
        data Doc redefined as an 'abstract syntax tree'
           along with the smart constructors for each type
           
        new layouts, flatten and pretty functions

-}
type Layout = String
data Doc    = Nil
            | Line
            | Text String
            | Nest Int Doc
            | Group Doc
            | Doc :<>: Doc     -- infix op as constructor

-- simple document definitions (smart constructors)
nil, line :: Doc
nil    = Nil         -- empty document
line   = Line        -- document with a single empty line

text :: String -> Doc
text s = Text s

nest :: Int -> Doc -> Doc
nest i x = Nest i x

group :: Doc -> Doc
group x = Group x

(<>) :: Doc -> Doc -> Doc
x <> y = x :<>: y

layouts :: Doc -> [Layout]
layouts x = layr [(0,x)]
    where
        layr [] = [""]
        layr ((i,Nil):ids)      = layr ids
        layr ((i,Line):ids)     = ['\n':replicate i ' ' ++ ls
                               | ls <- layr ids]
        layr ((i,Text s):ids)   = [s ++ ls | ls <- layr ids]
        layr ((i,Nest j x):ids) = layr ((i+j,x):ids)
        layr ((i,x :<>: y):ids) = layr ((i,x):(i,y):ids)
        layr ((i,Group x):ids)  = layr ((i,flatten x):ids) ++
                               layr ((i,x):ids)

flatten :: Doc -> Doc
flatten Nil        = Nil
flatten Line       = Text " "
flatten (Text s)   = Text s
flatten (Nest i x) = flatten x
flatten (x :<>: y) = flatten x :<>: flatten y
flatten (Group x)  = flatten x
                
-- return the shape of a layout (a list of the length of the lines
-- that make up the layout)               
shape :: Layout -> [Int]               
shape = map length . lines 

-- Examples

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

pretty w x = best w [( 0, x)] 
    where 
        best r [] = "" 
        best r (( i, x :<>: y): ids) = best r (( i, x):( i, y): ids) 
        best r (( i, Nil): ids) = best r ids 
        best r (( i, Line): ids) = '\n': replicate i ' '
                                ++ best (w-i) ids 
        best r (( i, Text s): ids) = s ++ best (r-length s) ids 
        best r (( i, Nest j x): ids) = best r (( i + j, x): ids) 
        best r (( i, Group x): ids) = better r 
                                       (best r (( i, flatten x): ids)) 
                                       (best r (( i, x): ids))
                                       
        better r lx ly = if fits r lx then lx else ly
        
        fits r _ | r < 0 = False
        fits r []        = True
        fits r (c:cs)    = if c == '\n' then True else fits (r-1) cs
                                                                           
ex3b = putStrLn $ pretty 30 $ para pg4      -- 0.00 secs
ex3c = putStrLn $ pretty 30 $ para pg1      -- 0.00 secs
ex3d = putStrLn $ pretty 30 $ para pg3      -- 0.00 secs
ex3e = putStrLn $ pretty 30 $ para pg2      -- 0.00 secs


                            
                 