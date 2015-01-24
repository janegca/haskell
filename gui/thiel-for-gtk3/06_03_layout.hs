{-
    Gtk API
    
        Layout      Graphics.UI.Gtk.Layout.Layout
        
        Infinite scrollable area containing child widgets and/or 
        a custom drawing.  Creates a blank background, allows you
        to place widgets wherever you like on that background.

    References:
    http://muitovar.com/gtk2hs/chap6-3.html
            
    To compile:  ghc 06_03_layout -o layout

-}


import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI         -- initialize windowing system
    
    -- create a top level window and set attributes
    window <- windowNew
    set window [windowTitle          := "Alphabet" , 
                windowDefaultWidth   := 350,
                windowDefaultHeight  := 350 ,  
                containerBorderWidth := 10]
                
    -- create a scrollable window and add it to the main window
    sw <- scrolledWindowNew Nothing Nothing
    set sw [scrolledWindowPlacement     := CornerBottomRight, 
         scrolledWindowShadowType       := ShadowEtchedIn,
         scrolledWindowHscrollbarPolicy := PolicyAutomatic,
         scrolledWindowVscrollbarPolicy := PolicyAutomatic ]
    containerAdd window sw

    -- create a layout and add it to the scrolled window
    lyt <- layoutNew Nothing Nothing
    layoutSetSize  lyt lytWidth lytHeight
    widgetModifyBg lyt StateNormal (Color 65535 65535 65535)
    containerAdd sw lyt     
    
    -- add coordinate labels (placed in four corners of the layout)
    addCoordLabels lyt
    
    -- create alphabet letters, placing them at the center of the layout
    -- in a circular pattern
    letters <- sequence $ map (labelNew . Just) alphabet
    sequence_ $ map (\x -> widgetModifyFg x StateNormal 
                                         (Color 0 0 45000)) letters

    let wnums = zip letters [0..]
    sequence_ $ map (putLetter lyt) wnums     
    
    -- register signal handler
    on window objectDestroy mainQuit
    
    -- start application
    widgetShowAll window
    mainGUI
     
-- Helper Functions ----------------------------------------------------

-- constant values 
lytWidth, lytHeight :: Int              -- layout size
lytWidth  = 800
lytHeight = 800

ctrX, ctrY :: Int                       -- center of layout coord values
ctrX = lytWidth  `div` 2
ctrY = lytHeight `div` 2               

radius :: Double                        -- letter circle radius
radius = 0.25 * (fromIntegral ctrX)

alphabet :: [String]                    -- alphabet letters
alphabet = map (\c -> [c]) ['A'..'Z']   -- turn each Char into a String

step :: Double                          -- letter spacing
step = (2 * pi)/(fromIntegral (length alphabet))
  
-- add coord labels to the layout  
addCoordLabels :: Layout -> IO ()
addCoordLabels lyt = do
    -- create the labels
    upL  <- labelNew (Just "(0,0)")
    upR  <- labelNew (Just ( "(" 
                          ++ (show (lytWidth - 55)) 
                          ++ ",0) "))
    dwnR <- labelNew (Just ( "(0," 
                          ++ (show (lytHeight -20)) 
                          ++ ")"))
    dwnL <- labelNew (Just ( "(" 
                          ++ (show(lytWidth - 75)) 
                          ++ "," 
                          ++ (show (lytHeight - 20)) 
                          ++ ") "))
  
    -- add the labels to the layout
    layoutPut lyt upL  0 0
    layoutPut lyt upR (lytWidth - 55)  0
    layoutPut lyt dwnR 0 (lytHeight - 20)
    layoutPut lyt dwnL (lytWidth - 75) (lytHeight - 20)    
    
-- put an alphabet letter on the screen    
putLetter :: Layout -> (Label, Int) -> IO ()
putLetter ltr (lbl, n) = do 
        layoutPut     ltr lbl (num2x n) (num2y n) 
        labelSetAngle lbl (letterAngle n)   
    where
        num2x, num2y :: Int -> Int      -- letter coordinates
        num2x n = ctrX + ( round $ radius * (cos (angle n)) )
        num2y n = ctrY + ( round $ radius * (sin (angle n)) )

        angle, letterAngle :: Int -> Double 
        angle num     = 1.5 * pi + (fromIntegral num) * step        
        letterAngle n = 270 - ((angle n) * (180.0 /pi))       
                       
