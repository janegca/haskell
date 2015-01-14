{-
    Gtk API
    
    initGui     Graphics.UI.General.General
    
        initGui :: IO [String]
    
        - must be called before any other Gtk function
        - initializes the GUI
        - ALL Gtk functions MUST be called from the thread that called
          initGui; if you want to make a GUI call from external
          Haskell code, you need to post it to the main GUI thread        
    
    windowNew   Graphics.UI.Gtk.Windows.Window
        
        windowNew :: IO Window
    
        create a new top level window 
        
        - windows can have a WindowType of WindowTopLevel or 
          WindowPopup
          
    set     System.Glib.Attributes
    
        set :: o -> [AttrOp p] -> IO ()
        
        Set a number of properties (attributes) for some object.
        'AttrOp p' is a data type whose constructors set or update an 
        object property.
        
        An AttrOp can take the forms:
            attr := newValue       -- assign new value
            attr :~ (a -> b)       -- apply an update function
            attr := (IO b)         -- assign the result of an IO action
            attr :~> (IO b)        -- apply an IO udpate function
            attr ::= (o ->         -- assign a value with the object as
                                      an argument
            attr ::~ (o -> a -> b) -- assign an update function with the
                                      object as an argument
                
        'set' will process a list of suc
        
        
    on      System.Glib.Signals     (see Signal (Event) Handling below)
    
    widgetShowAll       Graphics.UI.Gtk.Abstract.Widget
    
        widgetShowAll :: WidgetClass self => self -> IO ()
        
        Shows the widget and, if is a container, reursively shows
        all its child widgets
    
    mainQuit            Graphics.UI.Gtk.General.General
    
        mainQuit :: IO ()
        
            exit the main event loop (see below). Returns True
            effectively blocking all other events.
    
    mainGui             Graphics.UI.Gtk.General.General
    
        mainGui :: IO ()
        
            run the main event loop
    
            a seemingly infinite loop that sleeps until a signal event of
            some kind (mouse, keyboard, resize, focus, etc) happens,
            at which time the corresponding event handler is triggered.
            Event handlers can execute default or custom (user defined)
            behaviours.
     
    -- Signal (Event) Handling -------------------------------------------
    There's been a change, between v2 and v3, in the way 'signals' (events)
    are registered. In v2, individual functions existed for
    registering the events i.e. onDestroy, onClicked, onDelete, etc.
    
    In v3, a more generalized mechanism has been implemented; there is a
    'Signal' type that specifies the structure of valid signal (event)
    handler. Signal is defined in System.Glib.Signals as:
    
        newtype Signal object handler
        
    and the 'signals' are registered with GUI objects using the 'on'
    function (also declared in System.Glib.Signals). 
    
    You only need to register signals you are interested in catching. 
    i.e. we normally don't care if a text label gets a key event so we 
    don't register any keyboard events with it.
    
    Registering a signal with a GUI object can take one of two forms:
    
        on object signal $ do ...            -- handler takes no args
        on object signal $ \args -> do ...   -- handler takes arguments
                
    For example:
        One valid signal is:
        
            showSignal :: WidgetClass self => Signal self (IO ())
        
        The type signature tells us that any GUI object belonging to
        the WidgetClass can, potentially, receive a showSignal and
        that every showSignal (event) handler must have type IO ().
        
        If we want an object to do something whenever it is 'shown',
        we register a 'show' handler with the that object, for example:
        
            on window showSignal $ do putStrLn "shown!"
            
        or
        
            window `on` showSignal $ do putStrLn "shown!"
            
        So, every time our 'window' is 'shown' the text "shown!"
        will be output to the operating system console.
        
    Another valid Signal is:
    
        focus :: WidgetClass self => Signal self (DirectionType -> IO Bool)
        
    The type signature tells us that any object belonging to the 
    WidgetClass can receive a 'focus' signal and that every 'focus'
    signal (event) handler must have the type (DirectionType -> IO Bool)
    i.e. it must take one argument of the type 'DirectionType', perform
    some IO and return a boolean value.
    
    If we want a GUI object to do something every time it gains focus,
    we register a focus handler with that object, for example:
    
      window `on` focus $ \dirtype -> putStrLn "focused!" >> return False
        
    here, our handler is an anonymous (lambda) function that takes
    one argument (a DirectionType) and prints the text "focused!"
    to the console. This handler must also return a True or False value.
    
    If a signal (event) handler returns 'False' it indicates that it
    does not override other handlers i.e. we did our thing, now everything
    should go on as usual.  
    
    If a signal (event) handler returns 'True' it indicates we do want
    our action to override other handlers.
        
    There is a large class of Signals that require a specific 'EventM'
    type, rather than the general IO type.  We create signals and
    register them as before but we need to be aware of the fact that
    'EventM' is a monad that wraps another monad.

    It provides access to data in an event i.e. it encapsulates event 
    information and is defined in Graphics.UI.Gtk.Gdk.EventM as:
    
         type EventM t = ReaderT (Ptr t) IO
         
    Because it is declared with 'type' we know it is a synonym (alias)
    for another type: ReaderT (Ptr t) IO which is, itself, a monad. This
    means that we often have to use 'liftIO' to get at the contents of
    a signal of this type.
    
    For example, to register a handler for the 'deleteEvent'
    
    deleteEvent :: WidgetClass self => Signal self (EventM EAny Bool)
    
    we need to use 'liftIO' in our handler   
    
        window `on` deleteEvent $ do
           liftIO (putStrLn "closing")
           liftIO mainQuit
           return False    
    
    i.e. we have to bring our 'action' up into the current monad
    before we can execute it.
    
    Another example is the 'configureEvent' signal which is sent 
    whenever a window is resized. The EConfigure object holds the
    new window size.
    
        configureEvent :: WidgetClass self => 
                Signal self (EventM EConfigure Bool)
                
    Again, from the type signature we know that any object of the
    WidgetClass can receive a 'configureEvent' signal and any handler
    for the signal must have the type (EventM EConfigure Bool).
    
    If we want a GUI object to do something when it is resized then
    we must register a handler:
    
        window `on` configureEvent $ do
           (width, height) <- eventSize
           liftIO (putStrLn (show width ++ " x " ++ show height))
           return False    
 
    The handler extracts width and height information from the signal,
    writes it to the console and returns False to indicate it does
    not want to override other handlers.
    
    -------------------------------------------------------------------
    
    objectDestroy   Graphics.UI.Gtk.Abstract.Object
    
        objectDestroy :: Signal Self (IO ())
        
        signals that all references to this object should be released
        
        - an item with no references is available for 'finalization'
          i.e. garbage collection
          
        - destroyed windows are removed from the screen and they, along
          with all their children, are destroyed
        
    References:
    ----------
    http://hackage.haskell.org/package/gtk3  Gtk3 Package
    https://www.haskell.org/haskellwiki/Gtk2Hs/Tutorials/Intro - Signals
    http://muitovar.com/gtk2hs/chap2.html   - GtkChap2.hs

    --
    To compile:  ghc 01_hello -o hello
    
-}
import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
  initGUI               -- required, initializes the window system
  
  -- create a new top level window, default size 200x200
  window <- windowNew   
  set window [windowTitle := "Chapter 2"]  -- set a window attribute
  
  -- example event handling
  -- registration and execution (note that 'focus' is obtained before
  -- 'show')
  on window showSignal $ do putStrLn "shown!"
  window `on` focus $ \dirtype -> putStrLn "focused!" >> return False
  window `on` configureEvent $ do
   (width, height) <- eventSize
   liftIO (putStrLn (show width ++ " x " ++ show height))
   return False
   
  -- exit the main event loop, destroying the top level window
  -- and all its children
  on window objectDestroy mainQuit  
  
  widgetShowAll window  -- show all the widgets we've created
  mainGUI               -- required, starts the main GUI event loop