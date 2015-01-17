{-
    Gtk API     
    
    Label       Graphics.UI.Gtk.Display.Label
    
        A widget that displays a small amount of text.
        Normally used to label other widgets.
        
            labelNew :: GlibString string => Maybe string -> IO Label
        
        Labels may contain a mnemonic (activated with Alt+Letter)
        To create a mnemonic, place an underscore (_) in front
        of the letter to be used as a mnemonic. Mnemonics automatically
        activate the widget they are associated with.
        
            labelNewWithNmemonic :: GlibString string -> IO Label
        
        Labels may be styled with 'markup' 
        (see http://hackage.haskell.org/package/pango-0.13.0.5/docs/
                     Graphics-Rendering-Pango-Markup.html) 
                     
        [Note: errors in markup are caught at Runtime, not during compile]
                    
        Labels can be made selectable; selected labels copy their
        text to the clipboard.
        
        Labels can be wrapped, aligned and justified.
        
        see the API for all the methods available, there are quite a few.
    
    
    Frame       Graphics.UI.Gtk.Ornaments.Frame
    
        A widget that acts as a container, takes an optional label
        that can be aligned.  Only one constructor:
        
            frameNew :: IO Frame
            
        The label text can be set, retrieved, replaced.
        The frame has 5 shadow type settings:
                ShadowNone, ShadowIn, ShadowOut, ShadowEtchedIn,
                ShadowEtechedOut
            
        
    References:
    http://muitovar.com/gtk2hs/chap4-2.html
    
    Haikus quoted from X.J. Kennedy, Dana Gioia, 
        Introduction to Poetry, Longman, 1997
        
    To compile:  ghc 04_03_labels -o labels

-}


import Graphics.UI.Gtk

main:: IO ()
main = do
  initGUI       -- initialize windowing system
  
  -- create top level window and set attributes
  window  <- windowNew
  set window [windowTitle := "Labels", containerBorderWidth := 10]

  -- create the main layout
  mainBox    <- vBoxNew False 10
  containerAdd window mainBox
  
  -- create horizontal layout to hold column layouts
  colBox <- hBoxNew True 5
  boxPackStart mainBox colBox PackNatural 0
  
  -- build the left and right column layouts with their contents
  buildLeftCol  colBox
  buildRightCol colBox
 
  -- add button along bottom of window
  button      <- buttonNewWithMnemonic "Haiku _Clicked"
  boxPackEnd mainBox button PackNatural 20
  
  -- register signal handlers
  on button buttonActivated (putStrLn "Haiku button clicked...")
  on window objectDestroy mainQuit
  
  -- start application
  widgetShowAll window
  mainGUI
  
-- Helper functions ------------------------------------------------------  
buildLeftCol :: HBox -> IO ()
buildLeftCol cb = do
    -- create vertical layout and add to column layout
    layout <- vBoxNew False 10
    boxPackStart cb layout PackNatural 0
    
    -- create contents
    (phLabel, phFrame) <- myLabelWithFrameNew
    labelSetMarkup phLabel "<span fgcolor='blue'>Penny Harter</span>"
    
    (hk1Label, hk1Frame) <- myLabelWithFrameNew
    labelSetText hk1Label "broken bowl\nthe pieces\nstill rocking"
    miscSetAlignment hk1Label 0.0 0.0
    
    hsep <- hSeparatorNew
    
    (gsLabel, gsFrame) <- myLabelWithFrameNew
    labelSetMarkup gsLabel "<i><b>Gary Snyder</b></i>"
    
    (hk2Label, hk2Frame) <- myLabelWithFrameNew
    labelSetText hk2Label "After weeks of watching the roof leak\n \
                           \ I fixed it tonight\nby moving a single board"
    labelSetJustify hk2Label JustifyCenter
    
    -- add contents to layout
    boxPackStart layout phFrame  PackNatural 0
    boxPackStart layout hk1Frame PackNatural 0
    boxPackStart layout hsep     PackNatural 10
    boxPackStart layout gsFrame  PackNatural 0
    boxPackStart layout hk2Frame PackNatural 0
    
    
buildRightCol :: HBox -> IO ()
buildRightCol cb  = do
    -- create layout for the right column and add to column layout
    layout <- vBoxNew False 0
    boxPackStart cb layout PackNatural 0
    
    -- create contents
    (kiLabel, kiFrame) <- myLabelWithFrameNew
    labelSetText kiLabel "Kobayashi Issa"
    
    (bLabel, bFrame) <- myLabelWithFrameNew
    labelSetText bLabel "\n\n\n"
    
    (ogLabel, ogFrame) <- myLabelWithFrameNew
    labelSetText ogLabel "One Guy"
    frameSetLabel ogFrame "Title:"
    labelSetPattern ogLabel [3, 1, 3]
    
    (hk1Label, hk1Frame) <- myLabelWithFrameNew
    labelSetMarkup hk1Label "<span bgcolor='yellow'>only one guy and\nonly one fly trying to\n \
                           \ make the guest room do</span>"
    labelSetJustify hk1Label JustifyRight
    
    -- add contents to layout
    boxPackStart layout kiFrame  PackNatural 0
    boxPackStart layout bFrame   PackNatural 0
    boxPackStart layout ogFrame  PackNatural 10
    boxPackStart layout hk1Frame PackNatural 0
    
-- create a new frame with an empty label    
myLabelWithFrameNew :: IO (Label,Frame)
myLabelWithFrameNew = do
  label <- labelNew (Just "")     -- 'labelNew Nothing' produced an error
  frame <- frameNew
  containerAdd frame label
  frameSetShadowType frame ShadowOut
  return (label, frame)
    
  