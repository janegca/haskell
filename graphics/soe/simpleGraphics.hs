{-  
    Chapter 3 - Simple Graphics

    Reference:
        'The Haskell School of Expression' by Paul Hudak
        http://www.cs.yale.edu/homes/hudak/SOE/index.htm
-}
module SimpleGraphics where

import SOE


-- 'runGraphics' sets up system tasks necessary for graphic IO
main0 = runGraphics(
            do w <- openWindow "My First Graphics Program" (300,300)
               -- draw text at position given; coords map to lower-left
               -- corner
               drawInWindow w(text (100,200) "Hello World")
               -- forces a pause for user input (which is ignored)
               k <- getKey w
               closeWindow w
         )
         
-- loop until user presses 'space' key, then close window   
-- NOTE: had to modify 'getKey' in SOE.hs to get this to work    
spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w

main1 = runGraphics(
            do w <- openWindow "My First Graphics Program" (300,300)
               -- draw text at position given; coords map to lower-left
               -- corner
               drawInWindow w(text (100,200) "Hello World")
               -- close the window when the user presses spacebar
               spaceClose w
         )
                              
-- draw a red ellipse           
pic1 = withColor Red (ellipse (150,150) (300,200))

-- draw rectangle outline in blue
pic2 = withColor Blue (polyline [(100,50),  (200,50),
                                 (200,250), (100,250), (100,50)] )          
                                 
main2 
    = runGraphics(
        do w <- openWindow "Some Graphics Figures" (300,300)
           drawInWindow w pic1
           drawInWindow w pic2
           spaceClose w 
    )
       
-- Drawing Sierpinski's Triangle ----------------------------------------
fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size
    = drawInWindow w (withColor Blue
        (polygon[(x,y),(x+size,y),(x,y-size),(x,y)]))

minSize :: Int
minSize = 8

sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size
    = if size <= minSize
      then fillTri w x y size
      else let size2 = div size 2
           in do sierpinskiTri w x y size2
                 sierpinskiTri w x (y-size2) size2
                 sierpinskiTri w (x+size2) y size2
 
-- choose a starting size that is a power of 2 to make
-- the subdivisions look more uniform 
main3 = runGraphics(
    do w <- openWindow "Sierpinski's Triangle" (400,400)
       sierpinskiTri w 50 300 256 
       spaceClose w
    )
    
    
    