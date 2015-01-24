{-
    Gtk API
    
        EventBox
            A widget used to catch events for widgets which do not 
            have their own window.
            
            In this example, it is used to wrap 'images', allowing
            the app to capture user mouse clicks on the image.
        
        ButtonBox
            Used to group buttons and provide them with a single
            size property. You can set spacing and padding properties
            which will apply to all the buttons in the box.

    References:
    http://muitovar.com/gtk2hs/chap6-2.html
    Fish graphics from github repository
      idontgetoutmuch/gtk2hs/docs/tutorial/Tutorial_Port/Example_Code
            
    To compile:  ghc 06_02_evtAndBtnBoxes -o evtboxes

-}
import Graphics.UI.Gtk
import System.Random (randomRIO)
import Control.Monad.Trans (liftIO)

main :: IO ()
main= do
    initGUI         -- initialize windowing system
    
    -- create a top level window and set attributes
    window <- windowNew
    mainBox <- hBoxNew False 0
    set window [windowTitle          := "Slot Machine",
                containerBorderWidth := 10,
                windowDefaultWidth   := 350, 
                windowDefaultHeight  := 400,
                containerChild       := mainBox]      

    -- build GUI content
    let picFiles = ["./jacunda.gif", "./pacu.gif", "./tucunaream.gif"]
    slots <- buildSlots  mainBox picFiles
    buildBtnBox mainBox slots picFiles

    -- register window signal handler
    on window objectDestroy mainQuit
    
    -- start application
    widgetShowAll window
    mainGUI
    
-- GUI Builders ----------------------------------------------------------

-- create the 'slots'
buildSlots :: HBox -> [String] -> IO [(EventBox,Image)]
buildSlots mb pics = do
    -- create a vertical layout to hold the fish images for the slots
    vbox <- vBoxNew False 0     
    
    -- wrap each image in an event box and add it to the slots layout
    -- 'slots' has type [(EventBox,Image)]
    slots <- sequence (map (initEvent vbox) pics)
    
    -- add the same tooltip to each slot
    sequence_ $ map (myTooltip . fst) slots

    -- add the slots to the main window
    boxPackStart mb vbox PackGrow 0
    
    return slots
        
-- create the window buttons        
buildBtnBox :: HBox -> [(EventBox,Image)] -> [String] -> IO ()
buildBtnBox mb slots pics = do
    -- create the buttons
    resetBtn <- buttonNewWithLabel "Reset"
    quitBtn  <- buttonNewWithLabel "Quit"
    playBtn  <- buttonNewWithMnemonic "_Play"
        
    -- create a vertical ButtonBox to hold the buttons
    vbb <- vButtonBoxNew 
    set vbb [containerBorderWidth := 10]
    
    -- register button signal handlers
    on playBtn  buttonActivated (play slots pics)
    on resetBtn buttonActivated $ sequence_ (zipWith reset slots pics)
    on quitBtn  buttonActivated $ do 
            Just parent <- widgetGetParent mb
            widgetDestroy parent    -- parent is the main window
    
    -- add the buttons to the button box
    containerAdd vbb resetBtn
    containerAdd vbb quitBtn
    containerAdd vbb playBtn
    
    -- configure the button box
    set vbb [buttonBoxLayoutStyle               := ButtonboxStart, 
             (buttonBoxChildSecondary playBtn)  := True ]    
    
    -- add the button box to main window
    boxPackStart mb vbb PackGrow 0
    

-- Helper Functions ------------------------------------------------------

-- wrap an image in an EventBox
initEvent :: VBox -> FilePath -> IO (EventBox, Image)
initEvent vb picFile = do
    -- create an event box to hold the given image
    eb   <- eventBoxNew
    img  <- imageNewFromFile picFile
    
    -- set the event box attributes
    set eb [containerChild       := img, 
            containerBorderWidth := 10 ]
            
    -- set the event box states        
    widgetModifyBg eb StateNormal      (Color 0 35000 0)
    widgetModifyBg eb StateInsensitive (Color 50000 50000 50000)
    
    -- register a signal handler to 'freeze' the image when the left
    -- mouse button is pressed
    on eb buttonPressEvent $ do
        button <- eventButton
        case button of
            LeftButton -> liftIO (widgetSetSensitivity eb False) 
                                >> return False
            _ -> return False

    -- add the event box to the slots layout
    boxPackStart vb eb PackGrow 0
    
    -- return the event box and the image
    return (eb, img)
        
-- add a tooltip to the given event box        
myTooltip :: EventBox -> IO ()
myTooltip eb = do
    set eb [widgetTooltipText := 
             (Just "Click the Left Mouse Button to freeze the image.")]

-- Signal Handlers -------------------------------------------------------
-- return the images to their original slot positions
reset :: (EventBox, Image) -> FilePath -> IO ()
reset (eb, im) pf = do widgetSetSensitivity eb True                 
                       imageSetFromFile im pf  

-- randomly change the slot images                       
play :: [(EventBox, Image)] -> [FilePath] -> IO ()
play slots pics = do
    let n = length pics
    rands <- sequence $ replicate n (randomRIO (0::Int,(n-1)))
    sequence_ (zipWith display slots rands) 
          where
            display (eb, im) rn = do
                      state <- widgetGetState eb
                      if state == StateInsensitive 
                         then return ()
                         else imageSetFromFile im (pics !! rn)   
             