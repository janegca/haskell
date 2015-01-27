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
                  putStrLn (show k)
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
                              
                              
                              