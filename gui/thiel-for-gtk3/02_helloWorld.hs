--
-- References:
--   http://muitovar.com/gtk2hs/chap2.html - Hello World
--   https://github.com/PositronicBrain/Gtk2HsTutorial - GtkButton.lhs
--
-- To compile:  ghc 02_helloWorld -o hellow
--

import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = do
  initGUI
  
  -- create widgets
  window <- windowNew
  button <- buttonNew
  
  set window [windowDefaultWidth := 300, windowDefaultHeight := 300,
              containerChild := button,  containerBorderWidth := 10,
              windowTitle := "Hello World!"]
              
  -- 'buttonActivated' is sent when the button is pressed AND released
  on button buttonActivated $ do (hello button)
                                 
  on window objectDestroy mainQuit
  
  -- start GUI
  widgetShowAll window
  mainGUI