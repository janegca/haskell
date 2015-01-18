{-
    Gtk API
    
    Arrow           Graphics.UI.Gtk.Misc.Arrow
    
        The Arrow widget draws an arrow pointing in one of the four
        cardinal directions.  There is one constructor:
        
            arrowNew :: ArrowType -> ShadowType -> IO Arrow
            
        ArrowType can be one of:
            ArrowUp, ArrowDown, ArrowLeft, ArrowRight, ArrowNone
            
        ShadowType can be one of:
            ShadowNone, ShadowIn, ShadowOut, ShadowEtchedIn,
            ShadowEtechedOut
    
        The arrow's direction and shadow can both be changed by
        setting the arrowArrowType or arrowShadowType attributes.
    
    Tooltips
    
        The tooltip mechanism used in the original Thiel tutorial
        was deprecated in Gtk3.
        
        The simplest way to set a tooltip on a widget is to set the
        'widgetTooltipText' or 'widgetTooltipMarkup' attribute, both of 
        which takes a 'Maybe' value. When the attribute is set, the 
        'hasTooltip' attribute is automatically set to True and a default 
        tooltip signal handler is created.
        
        You can disable a tooltip by setting the 'widgetHasTooltip'
        attribute to False; to turn it back on, set value to True.
                

    References:
    http://muitovar.com/gtk2hs/chap4-4.html
        
    To compile:  ghc 04_04_arrowsAndTooltips -o arrows
-}

import Graphics.UI.Gtk

main:: IO ()
main = do
  initGUI       -- initialize windowing system
  
  -- create top level window and set attributes
  window  <- windowNew
  set window [windowTitle := "Arrow Tips",
              windowDefaultWidth := 200,
              windowDefaultHeight := 200,
              containerBorderWidth := 20]

  -- create the main layout
  tbl <- tableNew 5 5 True
  containerAdd window tbl
  
  -- build contents
  buildContents tbl
    
  -- register signal handlers
  on window objectDestroy mainQuit
  
  -- start application
  widgetShowAll window
  mainGUI
  
-- Helper functions ------------------------------------------------------  
buildContents :: Table -> IO ()
buildContents tbl = do
    -- create buttons
    b1 <- buttonNew
    b2 <- buttonNew
    b3 <- buttonNew
    b4 <- buttonNew
        
    -- create arrows and add to buttons
    a1 <- arrowNew ArrowLeft  ShadowEtchedIn   -- West
    a2 <- arrowNew ArrowUp    ShadowEtchedOut  -- North
    a3 <- arrowNew ArrowRight ShadowEtchedIn   -- East
    a4 <- arrowNew ArrowDown  ShadowEtchedOut  -- South
    
    containerAdd b1 a1
    containerAdd b2 a2
    containerAdd b3 a3
    containerAdd b4 a4
        
    -- set button tooltips
    set b1 [widgetTooltipText   := (Just "West")]
    set b2 [widgetTooltipText   := (Just "North")]
    set b3 [widgetTooltipMarkup := (Just "East")]
    set b4 [widgetTooltipMarkup := (Just "South")]
    
    -- turn tooltip for 'East' off
    set b3 [widgetHasTooltip := False]
        
    -- add buttons to layout
    tableAttachDefaults tbl b1 0 1 2 3
    tableAttachDefaults tbl b2 2 3 0 1
    tableAttachDefaults tbl b3 4 5 2 3
    tableAttachDefaults tbl b4 2 3 4 5
    