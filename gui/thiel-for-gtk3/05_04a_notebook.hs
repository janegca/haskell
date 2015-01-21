{-
    Gtk API
    
        A tabbed notebook widget. The tabs may be displayed along
        the top, bottom or either edge using 'notebookSetTabPos'.
        Use 'notebookSetScrollable' if you want users to be able
        to scroll through the tabs.
        
        There are a number of other available configuration options.
        
    
    [Note: this example uses Gtk stock images; there is no image
           for the StockId 'gtk-discard' so, while the app doesn't fail,
           a runtime warning is produced:
           
           Gtk-CRITICAL **: gtk_icon_set_render_icon_pixbuf: assertion 
           `icon_set != NULL' failed
    ]

    References:
    http://muitovar.com/gtk2hs/chap5-4.html
            
    To compile:  ghc 05_04a_notebook -o notebook

-}
import Graphics.UI.Gtk
import Data.Char (toUpper)
import Data.Text (unpack)

main :: IO ()
main= do
    initGUI         -- initialize windowing system
    
    -- create top level window and set attributes
    window <- windowNew
    set window [windowTitle         := "Notebook Example 1", 
                windowDefaultWidth  := 300,
                windowDefaultHeight := 200 ]

    -- create and configure notebook to display Gtk 'stock' images
    -- page tabs are scrollable and positioned alont the bottom of
    -- the pages
    ntbk <- notebookNew
    set ntbk [notebookScrollable := True, 
              notebookTabPos     := PosBottom]  -- position tabs on bottom

    stls <- stockListIds                        -- get stock image ids
    sequence_ (map (myNewPage ntbk) stls)
    
    -- add notebook to main window
    containerAdd window ntbk

    -- register signal handlers
    on ntbk switchPage (putStrLn . ((++)"Page: ") . show)
    on window objectDestroy mainQuit

    -- start application
    widgetShowAll window
    mainGUI

-- Helper Functions ------------------------------------------------------

-- creates a new page for each StockId, setting the page (tab) name
-- to the image name and setting the page content to the StockId's image
myNewPage :: Notebook -> StockId -> IO Int
myNewPage noteb stk = 
              do img     <- imageNewFromStock stk IconSizeDialog
                 pagenum <- notebookAppendPage noteb img (tabName stk)
                 return pagenum      
                 
-- each StockId is a Data.Text string in the form "gtk-imageName" 
-- tabName strips off the 'gtk-' prefix, returning the image name
-- in proper case, with the first letter capitalized
tabName :: StockId -> String
tabName st = (drop 3) (conv txt) where
                  txt = unpack st
                  conv (x:[]) = x:[]
                  conv (x:y:ys) | x == '-'  = (toUpper y):(conv ys)
                                | otherwise = x : (conv (y:ys))
                 