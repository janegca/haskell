{-
    Gtk API
    
    FileChooserDialog     Graphics.UI.Gtk.Selectors.FileChooserDialog
    
        A file chooser dialog for File/Open or File/Save mode. 
        
        Constructor:
            fileChooserDialogNew 
                :: GlibString string
                => Maybe string           -- window title (or default)
                -> Maybe Window           -- transient parent (or default)
                -> FileChooserAction      -- open or save
                -> [(string, ResponseId)] -- buttons and their response
                                          -- codes
                -> IO FileChooserDialog
                
        The 'FileChooserActions' are: 
            FileChooserActionOpen
            FileChooserActionSave
            
        The button 'string' values can be any string value or Gtk stock
        button values i.e. "gtk-open", "gtk-close", etc. along with
        their related response ids i.e. ResponseAccept, ResponseClose, etx.
        If you use the 'stock' names, the button will have the appropriate
        icons.
        
        The dialog also contains two samll '+' and '-' buttons under
        the file names; these are used to add 'bookmarks' or 'shortcuts'
        to the list file lists i.e. you can select a folder and then
        click the '+' button to add it to the selectable files on the
        left hand side of the window (similar creating a 'Favorites' in
        Window Filer Explorer; no 'new' folder is created on the drive).
        [Note: couldn't find any way to disable/remove this feature]
    
        In addition to adding buttons when creating a dialog, you can
        add other options and a preview pane (use the default or 
        customize one). Filters may also be added to determine which
        files are shown.
        
    When using dialogs, we normally make a 'runDialog' call,
    which starts a loop that waits for for user input. Once the user 
    clicks a button (or hits Enter) a 'response' is triggered; it's
    this response that we want to capture and act on.
    
    References:
    http://muitovar.com/gtk2hs/chap5-2.html
            
    To compile:  ghc 05_02_fileChooser -o fchoose

-}
import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    initGUI         -- initialize windowing system
                       
    -- create the file open dialog
    fod <- fileChooserDialogNew (Just "File Open Dialog")
                                Nothing
                                FileChooserActionOpen
                                -- create buttons with stock icons
                                [("gtk-open",   ResponseAccept),
                                 ("gtk-cancel", ResponseCancel),
                                 ("gtk-close",  ResponseClose)]
    
    -- add extra widgets/options
    addFileFilters fod
    
    mfSelect <- checkButtonNewWithLabel "Multiple File Selection"
    img      <- imageNew        -- for 'image preview' option
    
    fileChooserSetPreviewWidget fod img
    fileChooserSetExtraWidget   fod mfSelect
        
    -- register signal handlers
    on mfSelect toggled $ do state <- toggleButtonGetActive mfSelect
                             set fod [fileChooserSelectMultiple := state]
                                            
    on fod updatePreview $ 
        -- Note that only one filename is returned even if
        -- multiple file selections are allowed
        do file <- fileChooserGetPreviewFilename fod
           case file of
                Nothing    -> putStrLn "No File Selected"
                Just fpath -> imageSetFromFile img fpath  
                             
    -- start application 
    -- [Note: a mainGUI call is not needed for this example 
    --        as the 'dialogRun' function in 'runDialog' will
    --        create its own 'loop'.]
    runDialog  fod
    
-- creates the file dialog and waits for user input
runDialog :: FileChooserDialog -> IO ()    
runDialog fd = 
    do
        response <- dialogRun fd
        
        case response of
            ResponseAccept -> do 
                                putStrLn "You chose the following files:\n"
                                getFileNames fd
            ResponseCancel -> putStrLn "Dialog cancelled"
            ResponseClose  -> putStrLn "Dialog closed"

        widgetDestroy fd
            
-- Helper functions -----------------------------------------------------
addFileFilters :: FileChooserDialog -> IO ()
addFileFilters fd = do
    -- create filters for file extensions
    hsFiles <- fileFilterNew
    allFiles <- fileFilterNew
    
    -- add file filter pattern to the filter
    fileFilterAddPattern hsFiles "*.hs"     
    fileFilterAddPattern allFiles "*.*"
    
    -- give the filters names
    set hsFiles [fileFilterName  := "Haskell Source"]
    set allFiles [fileFilterName := "All Files"]
    
    -- add filters to the file dialog
    fileChooserAddFilter fd hsFiles
    fileChooserAddFilter fd allFiles   

-- get the selected file URIs, strip the 'file:///' prefix and print    
getFileNames :: FileChooserDialog -> IO ()
getFileNames fd = do
    fns   <- liftIO (fileChooserGetURIs fd)
    _     <- mapM (putStrLn . show . snd . (splitAt 8)) fns
    return ()
    
    
    