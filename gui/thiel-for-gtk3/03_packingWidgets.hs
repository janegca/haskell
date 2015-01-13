--
-- References:
--   http://muitovar.com/gtk2hs/chap3-1.html 
--   https://github.com/PositronicBrain/Gtk2HsTutorial - GtkButton.lhs
--
-- To compile:  ghc 03_packingWidgets -o packw
--
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  
  -- create the window and add a horizontal layout container
  --    layouts let the window know where to place widgets within
  --    the window
  window  <- windowNew  
  hbox    <- hBoxNew True 10  -- a horizontal layout box
  
  set window [windowDefaultWidth := 300,  windowDefaultHeight := 300,
              containerBorderWidth := 10, containerChild := hbox,
              windowTitle := "Packing Widgets"]
              
  -- create two buttons
  button1 <- buttonNewWithLabel "Button 1"
  button2 <- buttonNewWithLabel "Button 2"
          
  -- add buttons to the horizontal box layout
  -- boxPackStart will layout the buttons left to right 
  -- (use 'boxPackEnd' if you want the layout to run right to left)
  -- there are 3 packing options which determine what happens on a 
  -- window resize:
  --    PackNatural   - widgets retain their size and position
  --    PackGrow      - widgets will grow or shrink to fill the 
  --                    available space
  --    PackRepel     - widgets will be padded equally with unused 
  --                    available space
  -- the final parameter (set to 0 here) is used to set additional
  -- padding
  boxPackStart hbox button1 PackGrow 0
  boxPackStart hbox button2 PackGrow 0
  
  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI