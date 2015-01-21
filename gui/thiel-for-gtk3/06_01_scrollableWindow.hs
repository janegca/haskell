{-
    Gtk API
        
    ScrolledWindow      Graphics.UI.Gtk.Scrolling.ScrolledWindow
    
        Adds scrollbars to a widget window. Some widgets (TreeView,
        TextView, Layout) naturally support scrolling; others do 
        not. Those that don't support scrolling can be placed in
        a 'ViewPort'.
    

    References:
    http://muitovar.com/gtk2hs/chap6-1.html
            
    To compile:  ghc 06_01_scrollableWindow -o scrollwin

-}
import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO)
import Data.IORef 
import System.Random (randomRIO)

main:: IO ()
main= do
    initGUI         -- initialize the windowing system
    
    -- create top level window and set attributes
    window   <- windowNew
    mainBox <- vBoxNew False 0
    set window [ windowTitle          := "Guess a Number", 
                  windowDefaultWidth  := 300, 
                  windowDefaultHeight := 250,
                  containerChild      := mainBox]
                  
    -- build and add content
    buildContent mainBox window
    
    -- register the window signal handler
    on window objectDestroy mainQuit
                  
    -- start application
    widgetShowAll window
    mainGUI
    
-- GUI Builders ----------------------------------------------------------
buildContent :: VBox -> Window -> IO ()
buildContent mb win = do
    -- create main buttons and separators
    info <- labelNew (Just "Press \"New\" for a random number")
    
    play <- buttonNewFromStock stockNew
    quit <- buttonNewFromStock stockQuit
    set play [containerBorderWidth := 5]
    set quit [containerBorderWidth := 5]
    
    sep1 <- hSeparatorNew
    sep2 <- hSeparatorNew
    
    -- create a scrollable window with a grid of buttons
    (swin, tblBtns) <- liftIO $ buildScrollWindow
    
    -- create a mutable variable monad to hold a random number
    randstore <- newIORef 50
    
    -- register signal handlers
    randomButton info randstore play
    sequence_ (map (actionButton info randstore) tblBtns)  
    on quit buttonActivated (widgetDestroy win)
    
    -- layout the widgets and add to the main window
    boxPackStart mb info PackNatural 7
    boxPackStart mb sep1 PackNatural 7
    
    boxPackStart mb swin PackGrow 0
    boxPackStart mb sep2 PackNatural 7

    btnBox <- hBoxNew False 10
    boxPackStart btnBox play    PackNatural 0
    boxPackEnd   btnBox quit    PackNatural 0
    boxPackStart mb     btnBox  PackNatural 0
    
-- create a scrollable window with a grid of buttons labelled 1 to 100    
buildScrollWindow :: IO (ScrolledWindow, [Button])  
buildScrollWindow = do
    -- create a scrollable window
    swin  <- scrolledWindowNew Nothing Nothing
    
    -- create a table layout to hold numeric buttons
    table <- tableNew 10 10 True

    -- create buttons numbered 1 to 100 and lay them out as a grid
    buttonlist <- sequence (map numButton [1..100])
    let places = cross [0..9] [0..9]
    sequence_ (zipWith (attachButton table) buttonlist places)

    -- add the table layout to the scrollable window with a viewport
    scrolledWindowAddWithViewport swin table

    return (swin, buttonlist)

-- Helper functions -----------------------------------------------------
-- use the button's position in the button creation sequence as the 
-- button label
numButton :: Int -> IO Button
numButton n = do
    button <- buttonNewWithLabel (show n)
    return button

-- determine a button's row,col position given it's position
-- in the button creation sequence
cross :: [Int] -> [Int] -> [(Int,Int)]
cross row col = do 
    x <- row
    y <- col
    return (x,y)

-- add a button to the layout table at the given row,col position
attachButton :: Table -> Button -> (Int,Int) -> IO ()
attachButton tbl btn (x,y) = tableAttachDefaults tbl btn y (y+1) x (x+1)

--  the 'New' button has been activated, store a new random number
randomButton :: ButtonClass b => Label 
                              -> IORef Int 
                              -> b 
                              -> IO (ConnectId b)
randomButton inf rst b = 
    on b buttonActivated $ do 
        rand <- randomRIO (1::Int, 100) -- get new random number
        writeIORef rst rand             -- save it
        
        -- update the 'info' button with the new information
        set inf [labelLabel := "Ready"] 
        widgetModifyFg inf StateNormal (Color 0 0 65535)
                           
-- the user has clicked a table button, is their guess wrong or right?                           
actionButton :: ButtonClass b => Label 
                              -> IORef Int 
                              -> b 
                              -> IO (ConnectId b)
actionButton inf rst b = 
    on b buttonActivated $ do 
        -- get the number from the clicked table button
        label <- get b buttonLabel
        let num = (read label) :: Int  
        
        -- get the value of the stored random number
        rand <- readIORef rst
        
        -- compare the values and update the info label with the result
        case compare num rand of
            GT -> do set inf [labelLabel :=  "Too High"]
                     widgetModifyFg inf StateNormal (Color 65535 0 0)
                  
            LT -> do set inf [labelLabel := "Too Low"]
                     widgetModifyFg inf StateNormal (Color 65535 0 0)
                  
            EQ -> do set inf [labelLabel := "Correct"]
                     widgetModifyFg inf StateNormal (Color 0 35000 0)
                  
