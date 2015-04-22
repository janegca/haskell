{-
    Code Golf - Word Jumble
    
    Create a program that rearranges the characters of an input. 
    The first and  last letters of the input must stay in their original
    positions, but all other characters will be re-ordered. For instance: 
        
        Ryan blogs about ponies.
     -> Rayn bogls auobt pnioes.
     
    Source:
    http://java.dzone.com/articles/code-golf-word-jumble
    
    Solution has 280 characters (excl. blanks)

-}
jumble :: String -> String
jumble str = (unwords 
             . map (\w -> ((head w) : f (init (tail w)) ++ [last w]))
             . words $ init str) 
          ++ [last str]
    where        
        f []       = []
        f [y]      = [y]
        f (y:z:ys) = z : y : f (reverse ys)
        