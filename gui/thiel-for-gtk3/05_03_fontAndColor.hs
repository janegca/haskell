{-
    FontButton      Graphics.UI.Gtk.Selectors.FontButton
    
        A button to open a font selection dialog. The dialog displays
        sample text in the current font and allows the user to select 
        another.
        
    ColorButton     Graphics.UI.Gtk.Selectors.ColorButton
    
        A button to open a colour selection dialog. The dialog
        displays the currently selected colour and allows the
        user to select another.

    The Font and Color selector widgets are similar to the FileChooser 
    widget; all three can be started as a widget, dialog or button.
    In this example, the 'button' approach is used:
    
        fontButtonNew and colorButtonNew
        
    clicking on either button opens a corresponding 'selector' dialog.
    
    [Note: the following runtime warning occurs the first time the 
           colour button is clicked
           
          Gtk-CRITICAL **: gtk_box_reorder_child: assertion `GTK_IS_WIDGET 
                          (child)' failed
                          
           the application continues to work and the error does not
           re-occur on subsequent colour button clicks. ]
    
    References:
    http://muitovar.com/gtk2hs/chap5-2.html
            
    To compile:  ghc 05_03_fontAndColor -o fonts

-}
import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI     -- initialize windowing system
    
    -- create a top level window and set attributes
    window  <- windowNew
    mainBox <- vBoxNew False 0
    set window [windowTitle          := "Font and Color Selection",
                containerBorderWidth := 10,
                containerChild       := mainBox ]
    
    -- add content
    buildContent mainBox
    
    -- register signal handlers
    on window objectDestroy mainQuit
     
    -- start application
    widgetShowAll window
    mainGUI
    
-- GUI Builders ----------------------------------------------------------
buildContent :: VBox -> IO ()
buildContent mb = do
    -- create widgets
    quote <- labelNew (Just "How poor are they that have not \
                            \patience!\nWhat wound did ever heal but by \
                            \degrees?\nThou know'st we work by wit, \
                            \and not by witchcraft;\n \
                            \And wit depends on dilatory time.")
                          
    qtCite <- buildCiteLabel
    sep    <- hSeparatorNew
    fntBtn <- fontButtonNew
    clrBtn <- colorButtonNew
    
    -- register signal handlers
    onFontSet fntBtn $ do name  <- fontButtonGetFontName fntBtn
                          fdesc <- fontDescriptionFromString name
                          widgetModifyFont quote (Just fdesc)
                          putStrLn name

    onColorSet clrBtn $ do colour <- colorButtonGetColor clrBtn
                           widgetModifyFg quote StateNormal colour
                           putStrLn (show colour)   -- rgb values
    
    -- add widgets to main window
    boxPackStart mb quote  PackGrow     0
    boxPackStart mb qtCite PackNatural 10
    boxPackStart mb sep    PackGrow    10
    boxPackStart mb fntBtn PackGrow     0
    boxPackStart mb clrBtn PackGrow     0
     
-- build a label to hold the quote's source citation     
buildCiteLabel :: IO Label
buildCiteLabel = do
    -- create the label and font descriptor
    lbl  <- labelNew (Just "From Othello (II, iii, 376-379)")
    font <- fontDescriptionFromString "Courier Italic 10"
    
    -- set the label's font and alignment
    widgetModifyFont lbl (Just font)
    miscSetAlignment lbl 1.0 0.5
    
    return lbl
