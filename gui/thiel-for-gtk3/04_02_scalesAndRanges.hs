{-
    Gtk API
    
    Scale       Graphics.UI.Gtk.Abstract.Scale
    
        The Scale class is the base class for VScale and HScale
        (vertical and horizontal sliders) which can take a range
        of values defined by an Adjustment.
        
        The constructors for the two Scale types are:
        
            hScaleNew :: Adjustment -> IO HScale
            vScaleNew :: Adjustment -> IO VScale
            
        There are two other constructions which take bound values
        directly (rather than through an Adjustment value)
        
            hScaleNewWithRange 
                :: Double               min value
                -> Double               max value
                -> Double               step (tick size)
                -> IO VScale
                
            vScaleNewWithRange 
                :: Double               min value
                -> Double               max value
                -> Double               step (tick size)
                -> IO VScale

        [Note: these are not used in this demo]
                
    Adjustment  Graphics.UI.Gtk.Misc.Adjustment
    
        A value which has an associated lower and upper bound, 
        together with step and page increments, and a page size.

        Adjustments do not update their values; their owners respond
        to signals and handle any updates.
    
        adjustmentNew 
            :: Double               initial value
            -> Double               min (lower) value
            -> Double               max (upper) value
            -> Double               step increment
            -> Double               page increment
            -> Double               page size
            -> IO Adjustment
            
    CheckButton      Graphics.UI.Gtk.Buttons.CheckButton
    
        A discrete (single, ungrouped) toggle button.
        
        Constructors:
            checkButtonNew :: IO CheckButton
            checkButtonNewWithLabel :: string -> IO CheckButton
            checkButtonNewWithMnemonic :: string -> IO CheckButton
        
        Responds to the signal 'toggled'
        
    ComboBox    Graphics.UI.Gtk.MenuComboToolbar.ComboBox
    
        A ComboBox is a widget used to choose from a list of items.
        It uses the model-view pattern and the list display can be
        modified using a cell renderer.
        
        There are a number of constructors:
            comboBoxNew :: IO ComboBox
            comboBoxNewWithEntry :: IO ComboBox
            comboBoxNewText :: IO ComboBox
            comboBoxNewWithModel :: TableModelClass model
                                 => model       a TreeModel
                                 -> IO ComboBox
            comboBoxNewWithModelAndEntry :: TableModelClass model
                                         => model
                                         -> IO ComboBox
                                         
        The 'text' must be of type ComboBoxText which is an alias
        for the type 'Data.Text'.
        
        This demo uses 'comboBoxNewText' to create a combo box that
        allows users to select one of 'TOP, BOTTOM, LEFT, RIGHT'
        to position where the the digits for the sample sliders should be
        positioned.
    
    [Note: while this demo works, the best way to setup a ComboBox is
           likely to be by using a true mvc-pattern; the 'model' is 
           and cell renderer code is missing in this example.]
    
    References:
    http://muitovar.com/gtk2hs/chap4-2.html
    https://developer.gnome.org/gnome-devel-demos/unstable/scale.py.html.en
        
    To compile:  ghc 04_02_scalesAndRanges -o scales
-}
import Graphics.UI.Gtk
import Control.Monad
import Data.Text (Text, pack, unpack)

main :: IO ()
main = do
    
  initGUI       -- initialize the windowing system
  
  -- create top level window and set attributes
  window  <- windowNew
  set window [windowTitle := "Range Controls", 
              containerBorderWidth := 5,
              windowDefaultWidth := 350]
              
  -- create main layout control
  mainBox <- vBoxNew False 10
  containerAdd window mainBox
  containerSetBorderWidth mainBox 10
  
  -- create linked vertical and horizontal scales (sliders)
  adj1 <- adjustmentNew 0.0 0.0 101.0 0.1 1.0 1.0
  
  vsc  <- vScaleNew adj1
  hsc1 <- hScaleNew adj1
  hsc2 <- hScaleNew adj1
  
  -- create a layout, add layout and scales to the window
  vscBox  <- hBoxNew False 0
  boxPackStart mainBox vscBox PackGrow 0  
  boxPackStart vscBox vsc PackGrow 0
  
  hscBox <- vBoxNew False 0
  boxPackStart vscBox hscBox PackGrow 0
  boxPackStart hscBox hsc1 PackGrow 0
  boxPackStart hscBox hsc2 PackGrow 0
  
  -- create a checkbox to toggle the display of scale values
  chb <- checkButtonNewWithLabel "Display Value on Scale Widgets"
  toggleButtonSetActive chb True
  boxPackStart mainBox chb PackNatural 10
  
  -- create a combobox to select a scales preset positions
  -- ie positions scale digits to the top, left, bottom or right
  --    of the scale (slider)
  svpCb    <- makeScaleValuePosComboBox              
  svpLabel <- labelNew (Just "Scale Value Position")
  svpBox   <- hBoxNew False 10
  
  boxPackStart mainBox svpBox PackNatural 0
  boxPackStart svpBox svpLabel PackNatural 0
  boxPackStart svpBox svpCb PackGrow 10
  
  -- create a scale to control the number of decimal places
  -- displayed on the sample scales (sliders)
  adj2 <- adjustmentNew 1.0 0.0 5.0 1.0 1.0 0.0
  sdLabel <- labelNew (Just "Scale Digits: ")
  sdScale <- hScaleNew adj2
  
  sdBox <- hBoxNew False 0
  containerSetBorderWidth sdBox 10
  boxPackStart mainBox sdBox PackGrow 0
  boxPackStart sdBox sdLabel PackNatural 10
  boxPackStart sdBox sdScale PackGrow 0
  scaleSetDigits sdScale 0
  
  -- NOTE: the update policy functions included in the original tutorial
  --       are deprecated in Gtk3
  
  -- create a scale to adjust the page sizes of the three sample scales
  adj3     <- adjustmentNew 1.0 1.0 101.0 1.0 1.0 0.0
  sbpLabel <- labelNew (Just "Scrollbar Page Size: ")
  sbpScale <- hScaleNew adj3
  scaleSetDigits sbpScale 0

  sbpBox <- hBoxNew False 0
  containerSetBorderWidth sbpBox 10
  boxPackStart mainBox sbpBox PackGrow 0
  boxPackStart sbpBox sbpLabel PackNatural 0
  boxPackStart sbpBox sbpScale PackGrow 0
  
  -- register signal handlers
  on chb toggled $ do toggleDisplay chb [hsc1, hsc2]
                      toggleDisplay chb [vsc]
                      
  on svpCb changed $ do setScalePos svpCb hsc1
                        setScalePos svpCb hsc2
                        setScalePos svpCb vsc
          
  -- NOTE: the signals on Adjustment objects have NOT been converted
  --       from type IO (ConnectId Adjustment) to the Gtk3 Signal type   
  onValueChanged adj2 $ do setDigits hsc1 adj2
                           setDigits hsc2 adj2
                           setDigits vsc  adj2      

  onValueChanged adj3 $ do val <- adjustmentGetValue adj3
                           adjustmentSetPageSize adj1 val   

  on window objectDestroy mainQuit                           
  
  -- start application
  widgetShowAll window
  mainGUI
  
-- Helper Functions ------------------------------------------------------

-- names of preset scale positions as type ComboBoxText
top, bottom, left, right :: ComboBoxText
top    = pack "TOP"
bottom = pack "BOTTOM"
left   = pack "LEFT"
right  = pack "RIGHT"
  
-- create the ComboBox  
makeScaleValuePosComboBox :: IO ComboBox
makeScaleValuePosComboBox = do
    cb    <- comboBoxNewText
    
    let opts = [top, bottom, left, right]
    mapM (comboBoxAppendText cb) opts
    
    comboBoxSetActive  cb  0
    return cb
    
-- toggle the display of the digits on the three sample scales    
toggleDisplay :: ScaleClass self => CheckButton -> [self] -> IO ()
toggleDisplay b scls = mapM_ change scls
    where
        change sc = do st <- toggleButtonGetActive b
                       scaleSetDrawValue sc st
                         
-- set the position of the digits on the three sample scales                         
setScalePos :: ScaleClass self => ComboBox -> self -> IO ()
setScalePos cb sc = do
    ntxt <- comboBoxGetActiveText cb
    let sel = case ntxt of
                (Just x)  -> x
                Nothing   -> error "setScalePos: no position set"
                
    let pos = if sel == right then PosRight
              else if sel == bottom then PosBottom
                   else if sel == left then PosLeft
                        else PosTop     -- default to top position
                
    scaleSetValuePos sc pos
    
-- set the number of decimal places displayed in the scale digits    
setDigits :: ScaleClass self => self -> Adjustment -> IO ()
setDigits sc adj = do val <- get adj adjustmentValue
                      set sc [scaleDigits := (round val)]    
 