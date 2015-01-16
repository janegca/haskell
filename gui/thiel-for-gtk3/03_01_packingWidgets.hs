{-
    Gtk API
    
    -- Layouts -----------------------------------------------------------
    Window layouts tell the application how to 'lay out' the widgets on
    the screen and how to handle them when the window is resized, moved,
    etc.
    ----------------------------------------------------------------------
    
    HBox        Graphics.UI.Gtk.Layout.HBox
    
        HBox is a container that organizes child widgets in a row giving
        them the same height.
        
        It has one Constructor:
        
            hBoxNew :: Bool         -- if True, all children are given
                                       the same amount of space
                                       if False, extra space added at
                                       the end of the row
                    -> Int          -- number of pixels to place
                                       between each child
                    -> IO HBox      -- the new HBox
        
    There is a corresponding 'VBox' which organizes child widgets in
    columns.
    
    boxPackStart        Graphics.UI.Gtk.Abstract.Box
    
        boxPackStart :: (BoxClass self, WidgetClass child)
                     => self
                     -> child       -- widget to be added to the box
                     -> Packing     -- one of PackNatural, PackGrow,
                                       PackRepel
                     -> Int         -- pixel space between children
                     -> IO ()       
    
    will layout the children from left to right (use 'boxPackEnd' if you
    want the layout to run right to left)
    
    the 3 Packing options determine what happens on a window resize:
      PackNatural   - widgets retain their size and position
      PackGrow      - widgets will grow or shrink to fill the 
                      available space
      PackRepel     - widgets will be padded equally with unused 
                      available space

    References:
    http://muitovar.com/gtk2hs/chap3-1.html 
    https://github.com/PositronicBrain/Gtk2HsTutorial - GtkButton.lhs
    
    To compile:  ghc 03_01_packingWidgets -o packw
-}
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI   -- initialize the windowing system
  
  -- create a top level window and a horizontal layout container
  window  <- windowNew  
  hbox    <- hBoxNew True 10  -- a horizontal layout box, 10 pix padding
  
  -- set window attributes and add the layout box as a child
  set window [windowDefaultWidth := 300,  windowDefaultHeight := 300,
              containerBorderWidth := 10, 
              containerChild := hbox,           -- window child
              windowTitle := "Packing Widgets"]
              
  -- create two buttons
  button1 <- buttonNewWithLabel "Button 1"
  button2 <- buttonNewWithLabel "Button 2"
          
  -- add the buttons to the horizontal box layout
  -- Note: the buttons automatically become children of the window
  --       as the hbox is a child of the window
  boxPackStart hbox button1 PackGrow 0
  boxPackStart hbox button2 PackGrow 0
  
  -- register signal handlers
  on window objectDestroy mainQuit      -- exit the application
  
  -- start the application
  widgetShowAll window
  mainGUI