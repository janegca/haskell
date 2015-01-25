{-
    Gtk API
        
        Action          Graphics.UI.Gtk.ActionMenuToolbar.Action
        
            Represent opertations that can be performed when the
            user select a menu or toolbar item. Actions can be
            enabled or disabled as required.
        
        ActionGroup     Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup
        
            Allows grouping of menu and toolbar actions; basically
            maps menu/tool item names to Actions.
            
        UIManager       Graphics.UI.Gtk.ActionMenuToolbar.UIManager
        
            Constructs menus and toolbars from an XML file.
            
    NOTE:
        Could not get the accelerator keys working; tried the following
        in the hopes it might work but still no go (the 'Nothing' for
        the accelerator is supposed to activate the stock accelerator,
        which it does, 'CTRL+Q' showed up beside the 'Exit' menu item
        but the user action is still ignored)
        
     let exiAE = ActionEntry (pack "EXIA") (pack "Exit")
                         (Just stockQuit) Nothing
                         (Just (pack "Quit the application")) mainQuit
                         
     exia <- actionNew (actionEntryName exiAE)
                       (actionEntryLabel exiAE)
                       (actionEntryTooltip exiAE)
                       (actionEntryStockId exiAE)
                       
     actionGroupAddActionWithAccel agr exia $ actionEntryAccelerator exiAE
        

    References:
    http://muitovar.com/gtk2hs/chap7-1.html
            
    To compile:  ghc 07_01_menusAndActionItems -o menus

-}

import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI     -- initialize windowing system
    
    -- create top level window and set attributes
    window <- windowNew
    mainBox    <- vBoxNew False 0
    set window [windowTitle         := "Menus and Toolbars",
                windowDefaultWidth  := 450, 
                windowDefaultHeight := 200,
                containerChild      := mainBox]

    -- create menu item actions
    fma <- actionNew "FMA" "File" Nothing Nothing
    ema <- actionNew "EMA" "Edit" Nothing Nothing
    hma <- actionNew "HMA" "Help" Nothing Nothing

    -- create menu and toolbar actions
    newa <- actionNew "NEWA" "New"     (Just "Just a Stub") (Just stockNew)
    opna <- actionNew "OPNA" "Open"    (Just "Just a Stub") (Just stockOpen)
    sava <- actionNew "SAVA" "Save"    (Just "Just a Stub") (Just stockSave)
    svaa <- actionNew "SVAA" "Save As" (Just "Just a Stub") (Just stockSaveAs)
    exia <- actionNew "EXIA" "Exit"    (Just "Just a Stub") (Just stockQuit)
 
    cuta <- actionNew "CUTA" "Cut"   (Just "Just a Stub") (Just stockCut)    
    copa <- actionNew "COPA" "Copy"  (Just "Just a Stub") (Just stockCopy)
    psta <- actionNew "PSTA" "Paste" (Just "Just a Stub") (Just stockPaste)

    hlpa <- actionNew "HLPA" "Help"  (Just "Just a Stub") (Just stockHelp)

    -- create an action group and add the above items
    agr <- actionGroupNew "AGR"
    mapM_ (actionGroupAddAction agr) [fma, ema, hma]
    mapM_ (\ act -> actionGroupAddActionWithAccel agr act (Just "")) 
       [newa,opna,sava,svaa,cuta,copa,psta,hlpa]

    -- add an action accelerator for the 'Exit' item
    actionGroupAddActionWithAccel agr exia (Just "<Control>q")

    -- build the menu and toolbar
    ui <- uiManagerNew
    uiManagerAddUiFromString   ui uiDecl
    uiManagerInsertActionGroup ui agr 0

    maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
    let menubar = case maybeMenubar of
                       (Just x) -> x
                       Nothing -> error "Cannot get menubar from string." 
    
    maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
    let toolbar = case maybeToolbar of
                       (Just x) -> x
                       Nothing -> error "Cannot get toolbar from string." 
    

    -- add the menu and toolbar to the main window
    boxPackStart mainBox menubar PackNatural 0
    boxPackStart mainBox toolbar PackNatural 0
    
    -- disable the 'Cut' action
    actionSetSensitive cuta False

    -- register the signal handlers
    on exia actionActivated (widgetDestroy window)
    mapM_ prAct [fma,ema,hma,newa,opna,sava,svaa,cuta,copa,psta,hlpa]
    on window objectDestroy mainQuit

    -- start the application
    widgetShowAll window
    mainGUI
          
-- Menu in XML          
uiDecl=  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"NEWA\" />\
\              <menuitem action=\"OPNA\" />\
\              <menuitem action=\"SAVA\" />\
\              <menuitem action=\"SVAA\" />\
\              <separator />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\           <menu action=\"EMA\">\
\              <menuitem action=\"CUTA\" />\
\              <menuitem action=\"COPA\" />\
\              <menuitem action=\"PSTA\" />\
\           </menu>\
\            <separator />\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\           </menubar>\
\           <toolbar>\
\            <toolitem action=\"NEWA\" />\
\            <toolitem action=\"OPNA\" />\
\            <toolitem action=\"SAVA\" />\
\            <toolitem action=\"EXIA\" />\
\            <separator />\
\            <toolitem action=\"CUTA\" />\
\            <toolitem action=\"COPA\" />\
\            <toolitem action=\"PSTA\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

-- Helper Functions ------------------------------------------------------
prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = on a actionActivated  $ do name <- actionGetName a
                                     putStrLn ("Action Name: " ++ name)
                                                                          