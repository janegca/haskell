{-  
    Chapter 5 - Polymorphic Types and Higher-Order Functions

    Reference:
        'The Haskell School of Expression' by Paul Hudak
        http://www.cs.yale.edu/homes/hudak/SOE/index.htm
-}

import Draw
import Shape
import SOE

-- drawing concentric circles (bull's eye) using higher order
-- functions

-- note that 'mapM_' is equivalent to 'sequence_ (map aux css)'
drawShapes' :: Window -> [(Color, Shape.Shape)] -> IO ()
drawShapes' w css = mapM_ aux css
    where
        aux(c,s) = drawInWindow w (withColor c (shapeToGraphic s))
        
conCircles :: [Shape.Shape]      
conCircles = map circle[2.4,2.1..0.3]

coloredCircles :: [(Color,Shape.Shape)]
coloredCircles = zip[Black,Blue,Green,Cyan,Red,Magenta,Yellow,White]
                     conCircles
      
main = runGraphics(
        do w <- openWindow "Bull's Eye" (xWin,yWin)
           drawShapes' w coloredCircles
           spaceClose w
        )
        
-- Folds ----------------------------------------------------------------
--
-- reverse written with an accumulator
reverse' xs = rev [] xs
    where rev acc []     = acc
          rev acc (x:xs) = rev (x:acc) xs
          
-- reverse implemented as a fold left
reverse'' xs = foldl revOp [] xs
    where revOp a b = b : a
    
    