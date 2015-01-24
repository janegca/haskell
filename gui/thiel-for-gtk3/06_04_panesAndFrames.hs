{-
    Gtk API
    
        VPaned          Graphics.UI.Gtk.Layout.VPaned
        
        A paned window, stacks two layouts vertically (there is also
        an HPaned for two horizontal layouts).  The position of the
        divider between the two can be set (Note: there is no visible
        indicator that the divisor can be adjusted by the user, only
        see the handle when you mouse over the divider).
        
        AspectFrame     Graphics.UI.Gtk.Layout.AspectFrame
        
        Constrains a frame to a particular aspect ratio when it is
        resized.
        
    
    References:
    http://muitovar.com/gtk2hs/chap6-4.html
            
    To compile:  ghc 06_04_panesAndFrames -o panes

-}

import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI     -- initialize windowing system
    
    -- create a top level window and set attributes
    window <- windowNew
    set window [windowTitle := "Paned Window", 
                containerBorderWidth := 10,
                windowDefaultWidth := 400, 
                windowDefaultHeight := 400 ]

    -- create a vertically paned window (creates 2 panels)
    pane <- vPanedNew
    panedSetPosition pane 250   -- sets the pane divider position
    
    -- create an aspect frame
    frame <- aspectFrameNew 0.5 0.5 (Just 3.0)
    frameSetLabel frame "Aspect Ratio: 3.0"
    frameSetLabelAlign frame 1.0 0.0
    
    -- create a drawing area and colour it red
    drwArea <- drawingAreaNew
    widgetModifyBg drwArea StateNormal (Color 65535 0 0)

    -- create an editable text widget
    txtView <- textViewNew
    buf     <- textViewGetBuffer txtView
    
    -- add the widgets to layouts and main window
    containerAdd frame  drwArea     -- add draw area to the aspect frame
    panedAdd1    pane   frame       -- add the aspect frame to top pane
    panedAdd2    pane   txtView     -- add txtView to bottom pane
    containerAdd window pane        -- add pane to main window
    
    -- register signal handler
    on buf bufferChanged $ do cn <- textBufferGetCharCount buf
                              putStrLn (show cn)   
    on window objectDestroy mainQuit

    -- start application
    widgetShowAll window 
    mainGUI