{-
    Gtk API
    
    Entry       Graphics.UI.Gtk.Entry.Entry
        
        A single line text entry field.  The field will scroll if
        the entered text is longer than the displayed area.
        
        Two constructors:
            entryNew :: IO Entry
            entryNewWithBuffer :: EntryBufferClass buffer 
                               => buffer 
                               -> IO Entry
                               
            A single buffer may be shared amoung many widgets.
            
        There are a number of methods available to get/set text, 
        visibility, width, frame, length, etc.
    
    StatusBar   Graphics.UI.Gtk.Display.Statusbar
    
        An area to report minor messages, usually placed along
        the bottom of a window.  Gtk+ statusbars maintain a
        message stack with the top message being the one displayed.
        
        Any message added to the statusbar must specify a 'contextid'
        that is unique for each statusbar (may be retrieved using
        'statusBarGetContextId'.
        
        There is one constructor:
        
            statusbarNew :: IO Statusbar
            
        Messages are added to the message stack using:
            
            statusbarPush :: (StatusbarClass self, GlibString string)	
                          => self
                          -> ContextId
                          -> string             -- message to add
                          -> IO MessageId
                          
            [NOTE: statusbarPush does NOT recognize text 'markup';
                   passes it on as regular text]            
                          
        To display a message, call
            
            statusbarPop :: StatusbarClass self	
                         => self
                         -> ContextId
                         -> IO ()
                       
        To remove a message (without displaying it):
        
            statusbarRemove :: StatusbarClass self	
                            => self
                            -> ContextId
                            -> MessageId
                            -> ()
        
        Alternatively, you can remove all message by calling
        
            statusbarRemoveAll :: StatusbarClass self
                               => self
                               -> ContextId
                               -> IO ()
                        
        A status bar has no attributes.
    
    

    References:
    http://muitovar.com/gtk2hs/chap4-6.html
    
    For info on how to get the StatusBar label widget:
    https://stackoverflow.com/questions/24049015/line-breaks-in-statusbar
        
    To compile:  ghc 04_05_textEntryAndStatusBar -o txtentry

-}

import Graphics.UI.Gtk

main :: IO ()
main= do
  initGUI       -- initialize windowing system
  
  -- create top level window and set attributes
  window <- windowNew
  set window [windowTitle := "Text Entry", 
              containerBorderWidth := 10]
              
  -- create window contents
  txtField            <- entryNew
  infoButton          <- buttonNewFromStock stockInfo
  (sb, sbId, sbLabel) <- do createStatusBar
  
  -- create layouts and add contents
  mainBox  <- vBoxNew False 0
  entryBox <- hBoxNew False 0
  
  boxPackStart mainBox  entryBox   PackNatural 0
  boxPackStart entryBox txtField   PackNatural 5
  boxPackStart entryBox infoButton PackNatural 0
  boxPackStart mainBox  sb         PackNatural 0
  
  -- add main layout to window
  containerAdd window mainBox
  
  -- register signal handlers
  on txtField   entryActivated  (saveText txtField infoButton sb sbId)
  on infoButton buttonActivated (showMsgs sb sbId sbLabel infoButton)
  on window     objectDestroy   mainQuit
  
  -- disable the information button until we have some entry history
  widgetSetSensitivity infoButton False   
  
  -- start the application
  widgetShowAll window
  mainGUI

-- Helper function ------------------------------------------------------

-- create a status bar and extract the label widget
createStatusBar = do
  -- create the status bar and grab its label widget
  sb           <- statusbarNew
  sbId         <- statusbarGetContextId sb "Line"
  sbBox        <- statusbarGetMessageArea sb
  children     <- containerGetChildren sbBox
  let lbl  = castToLabel (head children)
  labelSetUseMarkup lbl True
  return (sb, sbId, lbl)

-- create palindrome messages
saveText :: Entry -> Button -> Statusbar -> ContextId -> IO ()
saveText fld b stk id = do
    txt <- entryGetText fld
    let msg | txt == reverse txt =  "\"" ++ txt ++ "\""
                                 ++ " is equal to its reverse"
            | otherwise          =   "\"" ++ txt ++ "\""
                                 ++  " is not equal to its reverse"
    widgetSetSensitivity b True
    
    -- doesn't recongize marked up text
    statusbarPush stk id msg
       
    return ()
    
-- display last palindrome message, if there is one 
showMsgs :: Statusbar -> ContextId -> Label -> Button -> IO ()
showMsgs sb id lbl b = do
    -- display last palindrome message
    statusbarPop sb id

    -- disable the information button if there are no more messages
    msg <- labelGetText lbl :: IO String  -- need to provide type to
                                          -- avoid 'ambiguous type' error
    if msg == "" then
       do widgetSetSensitivity b False    -- disable info button
          return ()
    else
       return ()

 