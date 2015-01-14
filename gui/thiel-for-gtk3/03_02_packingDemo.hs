{-
    Gtk API
    
    VBox        Graphics.UI.Gtk.Layout.VBox
    
        Organizes child widgets in a column. Are children are
        given the same width.
        
        Has one constructor that creates a new VBox:
        
            vBoxNew :: Bool         -- if True all children will have
                                       equal space allotment
                    -> Int          -- number of pixels between children
                    -> IO VBox      -- the new VBox
                    
        (see 03_01_packingWidgets.hs for info on corresponding HBox layout
         and details on boxPackStart)
    
    labelNew        Graphics.UI.Gtk.Display.Label
        
        labelNew :: Maybe string -> IO Label
        
            creates a new label using the given string.
            to create an empty label, pass in Nothing
    
    miscSetAlignment    Graphics.UI.Gtk.Abstract.Misc
    
        miscSetAlignment :: MiscClass self
                         => self
                         -> Float   -- xalign - horizontal alignment
                                                from left (0) to right (1)
                         -> Float   -- yalign - vertical alignment
                                                from top (0) to bottom (1)
                                                
        Sets widget alignment.
    
    HSeparartor       Graphics.UI.Gtk.Oranments.HSeparator
    
        A data type for a 'horizontal separator' used to group
        widgets in a window. Displays a shadowed, horizontal line.
        
        Has one constructor:
        
            hSeparatorNew :: IO HSeparator
            
                creates a new horizontal separator.

    References:
    http://muitovar.com/gtk2hs/chap3-2.html
    
    To compile:  ghc 03_02_packingDemo -o packDemo
-}

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI       -- initialize windows                           
  
  -- create a top level window and a vertical box layout
  window     <- windowNew
  vbox       <- vBoxNew False 0   -- extra space added to end, no padding
  
  -- set window attributes and add the vbox layout as a child
  set window [containerBorderWidth := 10,
              windowTitle := "Packing Demonstration",
              containerChild := vbox]
    
  -- create a new label and add it to the vertical layout box
  label1     <- labelNew (Just "hBoxNew False 0")
  miscSetAlignment label1 0 0              -- left-aligned
  boxPackStart vbox label1 PackNatural 0
  
  -- add three rows of buttons of buttons (each row is in
  -- its own horizontal layout box)
  
  -- btn row 1: buttons retain their size, extra space goes to the right
  box1       <- makeBox False 0 PackNatural 0
  boxPackStart vbox box1 PackNatural 0
  
  -- btn row 2: buttons retain their size, space between buttons allocated
  --            equally
  box2       <- makeBox False 0 PackRepel 0
  boxPackStart vbox box2 PackNatural 0
  
  -- btn row 3: buttons are resized and grow to fill available space
  --            equally
  box3       <- makeBox False 0 PackGrow 0
  boxPackStart vbox box3 PackNatural 0
  
  -- create a 'separator' and add it to the layout box
  sep1       <- hSeparatorNew
  boxPackStart vbox sep1 PackNatural 10
  
  -- create a second label and add it to the layout box
  label2     <- labelNew (Just "hBoxNew True 0")
  miscSetAlignment label2 0 0               -- left-aligned
  boxPackStart vbox label2 PackNatural 0
  
  -- add 3 more rows of buttons
  -- btn row 4: buttons retain size, space allocated equally
  box4       <- makeBox True 0 PackNatural 0
  boxPackStart vbox box4 PackNatural 0
  
  -- btn row 5: buttons retain size, space allocated equally
  box5       <- makeBox True 0 PackRepel 0
  boxPackStart vbox box5 PackNatural 0
  
  -- btn row 6: buttons sized equally, grow to fill available space
  box6       <- makeBox False 0 PackGrow 0
  boxPackStart vbox box6 PackNatural 0
  
  -- add another separator
  sep        <- hSeparatorNew
  boxPackStart vbox sep PackNatural 10
  
  -- add a horizontal layout to hold a 'Quit' button
  quitbox    <- hBoxNew False 0
  boxPackStart vbox quitbox PackNatural 0
  
  -- add a 'Quit' button to the quitbox layout
  quitbutton <- buttonNewWithLabel "Quit"
  boxPackStart quitbox quitbutton PackRepel 0
  
  -- register signal handlers
  on quitbutton buttonActivated mainQuit    -- exit application
  on window objectDestroy mainQuit          -- exit application
  
  -- start the application
  widgetShowAll window
  mainGUI

-- function to create horizontal rows of buttons
makeBox :: Bool -> Int -> Packing -> Int -> IO HBox
makeBox homogeneous spacing packing padding = do
  box     <- hBoxNew homogeneous spacing
  
  button1 <- buttonNewWithLabel "boxPackStart"
  boxPackStart box button1 packing padding
  
  button2 <- buttonNewWithLabel "box"
  boxPackStart box button2 packing padding
  
  button3 <- buttonNewWithLabel "button"
  boxPackStart box button3 packing padding
  
  button4 <- case packing of
                  PackNatural -> buttonNewWithLabel "PackNatural"
                  PackRepel   -> buttonNewWithLabel "PackRepel"
                  PackGrow    -> buttonNewWithLabel "PackGrow"
                  
  boxPackStart box button4 packing padding
  
  button5 <- buttonNewWithLabel (show padding)
  boxPackStart box button5 packing padding
  
  return box