{-
    Gtk API
    
        A calendar widget for selecting dates. The widget can include
        year, month and day selectors. The calendar can be displayed
        with or without headings, day names, week numbers.
        
        There is one constructor:
            
            calendarNew :: IO Calendar
            
        The new calendar is automatically created with the current date
        selected.
            
        There are very few options, other than those mentioned above,
        for controlling the look and feel of the widget i.e. days
        not actually in the active month are displayed in the same
        font weight as those in the active month, each week has the
        same background colour, etc. Although there are options to
        allow marking calendar days.
        
        If the calendar is packed using anything other than PackRepel
        the weeks block of the calendar will be separated from the 
        headings when the window is expanded.
        
        The signals retain their Gtk 2.0 form: onDaySelected, onNextYear,
        onMonthChanged, etc.
        
        Note: whenever a new month or year is selected, the currently
              selected day is automatically changed to the same day in
              the new month or new year, triggering the onDaySelected
              signal.
    
    References:
    http://muitovar.com/gtk2hs/chap5-1.html
            
    To compile:  ghc 05_01_calendar -o calendar

-}

import Graphics.UI.Gtk

main :: IO ()
main= do
    initGUI        -- initialize windowing system
     
    -- create new top level window and set attributes
    window  <- windowNew
    mainBox <- vBoxNew False 0
    set window [windowTitle         := "Calendar",
                windowDefaultWidth  := 200,
                windowDefaultHeight := 100,
                containerChild      := mainBox]
  
    -- create content
    calendar <- buildCalendarAndDisplayOptions mainBox
    resLabel <- buildResultLabel               mainBox
    
    -- set initial message
    showMsg calendar resLabel "No calendar selections yet."

    -- register signal handlers
    onDaySelected calendar 
        (showMsg calendar resLabel "Day Selected")
    onDaySelectedDoubleClick calendar
        (showMsg calendar resLabel "Double Click Day Selected")
        
    on window objectDestroy mainQuit
    
    -- start application
    widgetShowAll window
    mainGUI
    
-- GUI Builders ----------------------------------------------------------
buildCalendarAndDisplayOptions :: VBox -> IO Calendar
buildCalendarAndDisplayOptions mb = do
    -- create calendar
    cal <- calendarNew
    
    -- create vertical layout and options
    vbox <- vBoxNew False 0
    hOpt <- addDisplayOpt vbox "Show Heading"
    dOpt <- addDisplayOpt vbox "Show Day Names"
    mOpt <- addDisplayOpt vbox "No Month Change"
    wOpt <- addDisplayOpt vbox "Show Week Numbers"

    -- turn 'show heading' and 'show day names' options on
    set hOpt [toggleButtonActive := True]
    set dOpt [toggleButtonActive := True]    
    
    -- register signal handlers
    mySetOnToggled hOpt cal calendarShowHeading
    mySetOnToggled mOpt cal calendarNoMonthChange
    mySetOnToggled dOpt cal calendarShowDayNames
    mySetOnToggled wOpt cal calendarShowWeekNumbers
    
    -- create frame to hold calendar display options
    frame <- frameNew
    set frame [frameLabel           := "Display Options",
               containerBorderWidth := 10,
               frameLabelYAlign     := 0.5, 
               frameLabelXAlign     := 0.5,
               containerChild       := vbox]  

    -- create layout to hold calendar and display options 
    -- the components retain their original sizes    
    hbox <- hBoxNew False 0
    boxPackStart hbox cal    PackRepel 0
    boxPackStart hbox frame  PackRepel 0
    
    -- add to main window layout
    boxPackStart mb hbox PackRepel 0
    
    return cal
    
buildResultLabel :: VBox -> IO Label
buildResultLabel mb = do
    -- create label and put it in a frame
    lbl   <- labelNew (Just "")
    
    frame <- frameNew
    set frame [frameLabel := "Last Action:",
               containerBorderWidth := 10, 
               containerChild := lbl]
    
    -- add widget to main window
    boxPackStart mb frame PackGrow 0     
    
    return lbl
    
-- Helper Functions ------------------------------------------------------
addDisplayOpt :: VBox -> String -> IO CheckButton
addDisplayOpt box lbl = do
         cb <- checkButtonNewWithLabel lbl
         boxPackStart box cb PackGrow 5
         return cb

-- create signal handler for given check button         
mySetOnToggled :: CheckButton 
               -> Calendar 
               -> Attr Calendar Bool 
               -> IO (ConnectId CheckButton)
mySetOnToggled cb cl att = on cb toggled  $ do
         cbstate <- get cb toggleButtonActive
         set cl [att := cbstate]

-- display a message for the last action and the currently selected
-- date         
showMsg :: Calendar -> Label -> String -> IO ()
showMsg cal lbl str = do  
         (year, month, day) <- calendarGetDate cal
         labelSetText lbl $ str ++ "\n" ++ "Date = " 
                                ++ (show year) ++ "/" 
                                ++ (myshow (month +1))  -- month is 0 to 11
                                ++ "/" ++ (myshow day) 
    where myshow n | n <= 9    = '0':(show n)
                   | otherwise = show n
