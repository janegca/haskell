{-
    Gtk API
    
        ButtonClass     Graphics.UI.Gtk.Buttons.Button
        
            This class models common GUI buttons and has a number of 
            instances:
                FontButton, ColorButton, RadioButton, CheckButton,
                ToggleButton, LinkButton, VolumeButton, ScaleButton,
                Button
    
        buttonNew       Graphics.UI.Gtk.Buttons.Button
        
            buttonNew :: IO Button
            
            Creates a new Button widget. A Button can also be treated
            as a container.
            
        Button Attributes:
            buttonLabel             Graphics.UI.Gtk.Buttons.Button
                text of the label widget inside the button, if it
                has one; default is ""
            
        Window attributes:
            windowDefaultWidth      Graphics.UI.Gtk.Windows.Window      
            windowDefaultHeight 
                the default width and height of the window when it is 
                first shown; default values are (-1); 
                allowed values are anything >= (-1)
            
            windowDefaultHeight
                set the window width and height values
                
            containerChild      Graphics.UI.Gtk.Abstract.Container
                add a new child to the (window) container
                                    
    References:
      http://muitovar.com/gtk2hs/chap2.html - Hello World

    To compile:  ghc 02_helloWorld -o hellow
-}

import Graphics.UI.Gtk

-- set the given button's label text
hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = do
  initGUI       -- initialize the windowing system
  
  -- create widgets
  window <- windowNew
  button <- buttonNew
  
  -- set window attributes (properties)
  set window [windowDefaultWidth := 300, windowDefaultHeight := 300,
              containerChild := button,  containerBorderWidth := 10,
              windowTitle := "Hello World!"]
              
  -- 'buttonActivated' is sent when the button is pressed AND released
  on button buttonActivated $ do (hello button)
                           
  -- exit application                           
  on window objectDestroy mainQuit
  
  -- start the application
  widgetShowAll window
  mainGUI