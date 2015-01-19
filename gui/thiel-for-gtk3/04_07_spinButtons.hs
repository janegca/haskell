{-
    Gtk API
    
    SpinButton      Graphics.UI.Gtk.Entry.SpinButton
    
        User input widget for integer or floating point numbers. Based
        on the 'Adjustment' type (see 04_02_scalesAndRanges.hs)
        
        The widget has two constructors:
        
        spinButtonNew :: Adjustment     -- adjustment values
                      -> Double         -- climbrate - how far the
                                             value changes when the
                                             up/down arrows are clicked
                                             [step?]
                      -> Int            -- digits - the # of decimal
                                             places to display
                      -> IO SpinButton

        spinButtonNewWithRange :: Double        -- min value
                               -> Double        -- max value
                               -> Double        -- step 
                               -> IO SpinButton

            Builds the Adjustment value for you; a page increment of
            '10 * step' is the default; the precision of the spin
            button is equivalent to the step size, to change it,
            use 'spinButtonSetDigits'
            
        There are methods to configure the widget, set/get the adjustment
        values, increment, digits, range, values, etc. There are also
        methods to set an update policy (two choices, UpdateAlways or
        UpdateIfValid) as well as a method for spinning the button in
        specific directions including 'home' and 'end'.
        
        The signals retain the old Gtk methods.

    References:
    http://muitovar.com/gtk2hs/chap4-7.html
            
    To compile:  ghc 04_07_spinButtons -o spinbtns

-}

import Graphics.UI.Gtk

main:: IO ()
main = do
    initGUI     -- initialize windowing system
    
    -- create top level window, main layout, and set attributes
    window  <- windowNew
    mainBox <- vBoxNew False 0
    
    set window [windowTitle          := "Spin Buttons", 
                containerBorderWidth := 10,
                windowDefaultWidth   := 250, 
                windowDefaultHeight  := 200,
                containerChild       := mainBox] -- add main layout 
                
    -- create window contents
    buildSimpleButtons mainBox     
    buildMoreFeatures  mainBox    
                
    -- register window signal handlers
    on window objectDestroy mainQuit
    
    -- start application
    widgetShowAll window
    mainGUI
    
-- GUI Builders ----------------------------------------------------------

buildSimpleButtons :: VBox -> IO ()
buildSimpleButtons mb = do
    -- create a frame with a horizontal layout to hold the buttons
    hbox  <- hBoxNew False 0
    frame <- frameNew
    set frame [frameLabel       := "Simple SpinButtons", 
               containerChild   := hbox,
               frameLabelYAlign := 0.8, 
               frameShadowType  := ShadowOut]
    
    -- create spin buttons using helper function
    spinD <- myAddSpinButton hbox "Day:"   1.0    31.0
    spinM <- myAddSpinButton hbox "Month:" 1.0    12.0
    spinY <- myAddSpinButton hbox "Year:"  2000.0 2100.0
    set spinY [spinButtonValue := 2015]
    
    -- add spin buttons to main layout
    boxPackStart mb frame PackNatural 5    
    
buildMoreFeatures :: VBox -> IO ()
buildMoreFeatures mb = do
                   
    -- create spin buttons within a horizontal box layout 
    hbox      <- hBoxNew False 0
    
    value <- myAddSpinButton hbox "Value:" (-1000.0) 1000.0
    adj   <- adjustmentNew 0.0 (-100.0) 100.0 0.25 10.0 0.0
    spinButtonConfigure value adj 0.0 2
    
    decimals <- myAddSpinButton hbox "Decimal:" 0.0 10.0
    set decimals [spinButtonValue := 2.0]
    
    -- add check boxes for option selections
    ticks    <- checkButtonNewWithLabel "Snap to 0.25-ticks"
    numeric  <- checkButtonNewWithLabel "Numeric only input mode"
    wrap     <- checkButtonNewWithLabel "Wraparound at limits"
    
    -- register signal handlers
    onValueSpinned decimals $ 
        do newdig <- get decimals spinButtonValue
           set value [spinButtonDigits := (round newdig)]
           
    on ticks toggled $ do st <- get ticks toggleButtonActive
                          set value [spinButtonSnapToTicks := st]

    on numeric toggled $ do st <- get numeric toggleButtonActive
                            set value [spinButtonNumeric := st]

    on wrap toggled $ do st <- get wrap toggleButtonActive
                         set value [spinButtonWrap := st]           

    -- add more feature controls to a vertical layout
    vbox  <- vBoxNew False 5
    boxPackStart vbox hbox    PackNatural 0
    boxPackStart vbox ticks   PackNatural 0
    boxPackStart vbox numeric PackNatural 0
    boxPackStart vbox wrap    PackNatural 0
    
    -- create a frame to hold the more features layout
    frame <- frameNew
    set frame [frameLabel := "More Features",
               containerChild := vbox,
               frameLabelYAlign := 0.8, 
               frameShadowType:= ShadowOut]
        
    -- add the frame to the main window layout
    boxPackStart mb frame PackNatural 5

-- Helper functions ------------------------------------------------------
myAddSpinButton :: HBox -> String -> Double -> Double -> IO SpinButton
myAddSpinButton box name min max = do
    
    -- create spin button
    label <- labelNew (Just name)
    miscSetAlignment label 0.0 0.5
    
    spinb <- spinButtonNewWithRange min max 1.0
    
    -- add buttons to layout
    vbox  <- vBoxNew False 0
    boxPackStart vbox label PackNatural 0
    boxPackStart vbox spinb PackNatural 0
    boxPackStart box  vbox  PackRepel 0
    
    return spinb    