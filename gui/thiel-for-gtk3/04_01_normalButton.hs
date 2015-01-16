{-
    Gtk API
    
    Image
        A data type for a widget that displays an image
        
        Has a number of constructors, the simplest to use being:
    
    imageNewFromFile        Graphics.UI.Gtk.Display.Image
    
        imageNewFromFile :: fp -> IO Image
        
            creates a new image using the given file
            if the file contains an animation, the animation is loaded
            if the file cannot be found, a default 'broken image link'
            image is shown
            
    Other constructors:
        imageNewFromPixBuf :: PixBuf -> IO Image
        imageNewFromAnimation :: PixbufAnimationClass animation 
                              => animation -> IO Image
        imageNewFromStock :: StockId -> IconSize -> IO Image
        imageNew :: IO Image
        imageNewFromIconName :: GlibString string => string
                             -> IconSize -> IO Image
    
    References:
    http://muitovar.com/gtk2hs/chap4-1.html
    
    info.xpm file
    https://github.com/idontgetoutmuch/gtk2hs under docs/tutorial-port   
    
    To compile:  ghc 04_01_normalButton -o nbutton
-}

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI           -- initialize the windowing system
  
  -- create top level window and set attributes
  window <- windowNew
  set window [windowTitle := "Pix",
              containerBorderWidth := 10]
              
  -- create a normal button with an icon and text and add to window          
  button <- buttonNew  
  box    <- labelBox "info.xpm" "cool button"
  
  containerAdd button box       -- Note: buttons can act as containers
  containerAdd window button
  
  -- register signal handlers
  on button buttonActivated $ do (putStrLn "button clicked")
  on window objectDestroy mainQuit
  
  -- start the application
  widgetShowAll window
  mainGUI

-- create a horizontal layout box with an image and label
labelBox :: FilePath                -- location of image file 
         -> String                  -- text for label
         -> IO HBox
labelBox fn txt = do
  box   <- hBoxNew False 0
  set box [containerBorderWidth := 2]
  
  image <- imageNewFromFile fn      -- get the image to be displayed
  label <- labelNew (Just txt)      -- can also be Nothing
 
  boxPackStart box image PackNatural 3
  boxPackStart box label PackNatural 3
  return box