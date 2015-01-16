{-
    Gtk API
    
    RadioButton     Graphics.UI.Gtk.Buttons.RadioButton
    
        A choice from multiple check buttons.
        A single radio button acts like a check button but, when
        radio buttons are grouped together the selection of one
        automatically deselects all other radio buttons in the same group.
        
        The type has a number of constructors:
        
        radioButtonNew :: IO RadioButton
        radioButtonNewWithLabel :: GlibString string 
                                => string               -- label text
                                -> IO RadioButton
                                
        radioButtonNewWithMnemoic 
            :: GlibString string => string          -- label text with an
                                                       underscore in front
                                                       of letter to act as
                                                       the mnemonic
                                 -> IO RadioButton
                                 
        radioButtonNewFromWidget :: RadioButton    -- another radio button
                                                      who this one is to be
                                                      grouped with
                                 -> IO RadioButton
                                 
        radioButtonNewWithLabelFromWidget
            :: GlibString string => RadioButton     -- another radio button
                                                       this button is to
                                                       grouped with
                                 -> string          -- text for label
                                 -> IO RadioButton
                                 
    Radio button groupings can be queried/changed by calling:
        
        radionButtonSetGroup :: RadioButton 
                             -> RadioButton   -- radio button that the
                                                 first button is to be
                                                 grouped with
                             -> IO ()
                             
        radtioButtonGetGroup :: RadioButton 
                             -> IO [RadioButton]  - a list of other buttons
                                                    in the same group

    Radio buttons can be set programmatically calling:
    
        toggleButtonSetActive 
            :: ToggleButtonClass self => self
                                      -> Bool       -- True to select
                                      -> IO ()
                                      
    Note:
        RadioButton and CheckButton are instances of the type class
        ToggleButtonClass so all the methods of the ToggleButtonClass
        are available to them.
    
    References:
    http://muitovar.com/gtk2hs/chap4-1.html
        
    To compile:  ghc 04_01b_radioButtons -o rbuttons

-}

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI       -- initialize the windowing system
  
  -- create top level window and set attributes
  window  <- windowNew
  set window [windowTitle := "Radio Button", containerBorderWidth := 5,
              windowDefaultWidth := 200, windowDefaultHeight := 150]
              
  -- create vertical layout boxes
  box1    <- vBoxNew False 0            -- main layout
  box2    <- vBoxNew False 10           -- to hold radio buttons
  
  containerAdd window box1              -- add main layout to window
  containerSetBorderWidth box2 10       -- size border on 2nd box layout
  
  boxPackStart box1 box2 PackNatural 0  -- box2 is child of box1
  
  -- create the radio buttons
  button1 <- radioButtonNewWithLabel "button 1"
  boxPackStart box2 button1 PackNatural 0
  
  button2 <- radioButtonNewWithLabelFromWidget button1 "button 2"
  boxPackStart box2 button2 PackNatural 0
  
  button3 <- radioButtonNewWithLabelFromWidget button2 "button 3"
  boxPackStart box2 button3 PackNatural 0
  
  -- set the initial active radio button
  toggleButtonSetActive button2 True
  
  -- add a separator to the main layout
  sep     <- hSeparatorNew
  boxPackStart box1 sep PackNatural 0    -- sep child of box1
  
  -- create a third vertical layout box to hold the 'close' button
  box3    <- vBoxNew False 10
  containerSetBorderWidth box3 10
  boxPackStart box1 box3 PackNatural 0    -- box3 child of box1
  
  -- create the 'close' button and add it to the layout
  closeb  <- buttonNewWithLabel "close"
  boxPackStart box3 closeb PackNatural 0  -- close button child of box3
  
  -- register signal handlers
  -- the 'toggled' signal is emitted when a radio button is selected
  on button1 toggled (setRadioState button1)  
  on button2 toggled (setRadioState button2)
  on button3 toggled (setRadioState button3)

  on closeb buttonActivated mainQuit
  on window objectDestroy mainQuit
  
  -- start application
  widgetShowAll window
  mainGUI

-- display the state change that occurs when a radio button is selected
setRadioState :: RadioButton -> IO ()
setRadioState b = do
  state <- toggleButtonGetActive b
  label <- get b buttonLabel
  putStrLn ("State of " ++ label ++ " now is " ++ (show state))
  