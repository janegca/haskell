--
-- References:
--   http://muitovar.com/gtk2hs/chap2.html   - GtkChap2.hs
--   https://github.com/PositronicBrain/Gtk2HsTutorial - HelloWorld2.lhs
--
-- To compile:  ghc 01_hello -o hello
--
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI               -- required, initializes the window system
  
  -- create a new top level window, default size 200x200
  window <- windowNew   
  set window [windowTitle := "Chapter 2"]  -- set a window attribute
  
  -- added to prevent a hang on window close
  on window objectDestroy mainQuit
  
  widgetShowAll window  -- show all the widgets we've created
  mainGUI               -- required, starts the main GUI event loop