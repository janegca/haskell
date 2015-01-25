{-
    Gtk API
        
        Toggle or Radio button actions are created in a manner
        similar to regular menu/toolbar items. In this example,
        the actions themselves are created using the 'RadioActionEntry'
        and 'ToggleActionEntry' data types, both of which are defined
        in Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup.
    
    References:
    http://muitovar.com/gtk2hs/chap7-2.html
            
    To compile:  ghc 07_02b_radioActions -o radios

-}

import Graphics.UI.Gtk
import Data.Text (pack, unpack)

main :: IO ()
main= do
    initGUI         -- initialize windowing system
    
    -- create top level window and set attributes
    window <- windowNew
    set window [windowTitle         := "Radio and Toggle Actions",
                windowDefaultWidth  := 400,
                windowDefaultHeight := 200 ]
 
    -- create menu actions for 'highlight mode'
    mhma <- actionNew "MHMA" "Highlight\nMode" Nothing Nothing
    msma <- actionNew "MSMA" "Source"          Nothing Nothing
    mmma <- actionNew "MMMA" "Markup"          Nothing Nothing  

    -- create an action group for the 'highlight' mode items
    agr1 <- actionGroupNew "AGR1"
    mapM_ (actionGroupAddAction agr1) [mhma,msma,mmma]
    actionGroupAddRadioActions agr1 hlmods 0 myOnChange

    -- create actions for 'view' choices
    vima <- actionNew "VIMA" "View" Nothing Nothing          

    -- create action group for view items
    agr2 <- actionGroupNew "AGR2"
    actionGroupAddAction        agr2 vima
    actionGroupAddToggleActions agr2 togls

    -- build the menus and toolbars
    uiman <- uiManagerNew
    uiManagerAddUiFromString uiman uiDef1
    uiManagerInsertActionGroup uiman agr1 0

    uiManagerAddUiFromString uiman uiDef2
    uiManagerInsertActionGroup uiman agr2 1

    mayMenubar <- uiManagerGetWidget uiman "/ui/menubar"
    let mb = case mayMenubar of 
                   (Just x) -> x
                   Nothing -> error "Cannot get menu bar."

    mayToolbar <- uiManagerGetWidget uiman "/ui/toolbar"
    let tb = case mayToolbar of 
                   (Just x) -> x
                   Nothing -> error "Cannot get tool bar."

    -- add the menu and toolbar to the main window               
    box <- vBoxNew False 0
    containerAdd window box
    boxPackStart box mb PackNatural 0
    boxPackStart box tb PackNatural 0

    -- register signal handlers
    on window objectDestroy mainQuit
    
    -- start the application
    widgetShowAll window
    mainGUI

-- Helper Functions -------------------------------------------------------    
hlmods :: [RadioActionEntry]
hlmods = [
     RadioActionEntry (pack "NOA") (pack "None")    
                       Nothing Nothing Nothing 0,   
     RadioActionEntry (pack "SHA") (pack "Haskell")
                      (Just stockHome)  Nothing Nothing 1, 
     RadioActionEntry (pack "SCA") (pack "C")
                      Nothing Nothing Nothing 2,
     RadioActionEntry (pack "SJA") (pack "Java")
                       Nothing Nothing Nothing 3,
     RadioActionEntry (pack "MHA") (pack "HTML")
                       Nothing Nothing Nothing 4,
     RadioActionEntry (pack "MXA") (pack "XML")
                       Nothing Nothing Nothing 5]

myOnChange :: RadioAction -> IO ()
myOnChange ra = do val <- radioActionGetCurrentValue ra
                   putStrLn ("RadioAction " ++ (show val) ++ " now active.")

uiDef1 = " <ui> \
\           <menubar>\
\              <menu action=\"MHMA\">\
\                 <menuitem action=\"NOA\" />\
\                 <separator />\
\                 <menu action=\"MSMA\">\
\                    <menuitem action= \"SHA\" /> \
\                    <menuitem action= \"SCA\" /> \
\                    <menuitem action= \"SJA\" /> \
\                 </menu>\
\                 <menu action=\"MMMA\">\
\                    <menuitem action= \"MHA\" /> \
\                    <menuitem action= \"MXA\" /> \
\                 </menu>\
\              </menu>\
\           </menubar>\
\            <toolbar>\
\              <toolitem action=\"SHA\" />\
\            </toolbar>\
\           </ui> "            

togls :: [ToggleActionEntry]
togls = let mste = ToggleActionEntry (pack "MST") (pack "Messages")
                              Nothing Nothing Nothing (myTog mste) False   
            ttte = ToggleActionEntry (pack "ATT") (pack "Attributes")
                              Nothing Nothing Nothing (myTog ttte)  False 
            erte = ToggleActionEntry (pack "ERT") (pack "Errors")
                      (Just stockInfo) Nothing Nothing (myTog erte)  True 
        in [mste,ttte,erte]

myTog :: ToggleActionEntry -> IO ()
myTog te = putStrLn ("The state of " ++ unpack (toggleActionName te) 
                      ++ " (" ++ unpack (toggleActionLabel te) ++ ") " 
                      ++ " is now " ++ (show $ not (toggleActionIsActive te)))
uiDef2 = "<ui>\
\          <menubar>\
\            <menu action=\"VIMA\">\
\             <menuitem action=\"MST\" />\
\             <menuitem action=\"ATT\" />\
\             <menuitem action=\"ERT\" />\
\            </menu>\
\          </menubar>\
\            <toolbar>\
\              <toolitem action=\"MST\" />\
\              <toolitem action=\"ERT\" />\
\            </toolbar>\
\         </ui>"

