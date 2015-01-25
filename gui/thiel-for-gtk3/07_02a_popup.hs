{-
    Gtk API
    
        Popup menus are buil the same as normal menus, the only
        difference being the way they are identified in the
        XML string, as a '<popup>' element, and, the way they
        are called, 'menuPopup' (we don't pack and add them to the
        main window).
    
    References:
    http://muitovar.com/gtk2hs/chap7-2.html
            
    To compile:  ghc 07_02a_popup -o popup

-}
import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO)

main :: IO ()
main= do
    initGUI         -- initialize windowing system
    
    -- create new top level window and set attributes
    window <- windowNew
    set window [windowTitle         := "Right-Click to see Popup Menu",
                windowDefaultWidth  := 450,
                windowDefaultHeight := 150 ]

    -- create actions
    eda <- actionNew "EDA" "Edit"    Nothing Nothing
    pra <- actionNew "PRA" "Process" Nothing Nothing
    rma <- actionNew "RMA" "Remove"  Nothing Nothing
    saa <- actionNew "SAA" "Save"    Nothing Nothing

    -- create an ActtionGroup
    agr <- actionGroupNew "AGR1" 
    mapM_ (actionGroupAddAction agr) [eda,pra,rma,saa]

    -- build the menu
    uiman <- uiManagerNew
    uiManagerAddUiFromString   uiman uiDecl
    uiManagerInsertActionGroup uiman agr 0

    maybePopup <- uiManagerGetWidget uiman "/ui/popup"
    let pop = case maybePopup of 
                   (Just x) -> x
                   Nothing -> error "Cannot get popup from string"

    on window buttonPressEvent $ do
       button <- eventButton
       case button of
           RightButton -> liftIO(menuPopup (castToMenu pop) Nothing) 
                            >> return False
           _ -> return False
     
    -- register signal handlers
    mapM_ prAct [eda,pra,rma,saa]
    on window objectDestroy mainQuit

    -- start the application
    widgetShowAll window
    mainGUI

-- XML string for menu
uiDecl = "<ui> \
\          <popup>\
\            <menuitem action=\"EDA\" />\
\            <menuitem action=\"PRA\" />\
\            <menuitem action=\"RMA\" />\
\            <separator />\
\            <menuitem action=\"SAA\" />\
\          </popup>\
\        </ui>"   

prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = on a actionActivated $ do name <- actionGetName a
                                    putStrLn ("Action Name: " ++ name)