{-
    Gtk API
    
        Table   Graphics.UI.Gtk.Layout.Table
        
            Data type for a table/grid layout; allows widgets to be
            placed in rows and columns.
            
            Layout starts at top (0) left (0)
    
                 0          1          2               n
                0+----------+----------+ ..... +-------+
                 |          |          | ..... |       |
                1+----------+----------+ ..... +-------+
                 |          |          | ..... |       +
                2+----------+----------+ ..... +-------+
                       .                   .
                       .                   .
               m +----------+----------+ ...... +------+(m,n)
               
    Widgets are added to the layout using 'tableAttach' which has many 
    options:
            
    tableAttach :: (TableClass self, WidgetClass child)
      => self            -- self         - The table.
      -> child           -- child        - The widget to add.
      -> Int             -- leftAttach   - The column number to attach the 
                                           left side of a child widget to.
      -> Int             -- rightAttach  - The column number to attach the
                         --                right side of a child widget to.
      -> Int             -- topAttach    - The row number to attach the
                         --                top of a child widget to.
      -> Int             -- bottomAttach - The row number to attach the
                         --                bottom of a child widget to.
      -> [AttachOptions] -- xoptions     - Used to specify the properties 
                         --                of the child widget when the 
                         --                table is resized.
      -> [AttachOptions] -- yoptions     - The same as xoptions, except
                         --                this field determines behaviour
                         --                of vertical resizing.
      -> Int             -- xpadding     - An integer value specifying the
                         --                padding on the left and right of
                         --                the widget being added to the 
                         --                table.
      -> Int             -- ypadding     - The amount of padding above and
                         --                below the child widget.
      -> IO ()    
      
    The 'xoptions' and 'yoptions' are packing options:
      
        Fill    - widget expands to fill available space
        Shrink  - shrink widget to available space
        Expand  - causes the table to expand to fill all available space

    Or you can add a widget using 'tableAttachDefaults', which takes
    fewer options.
    
    tableAttachDefaults :: (TableClass self, WidgetClass widget)
      => self   -- self         - The table.
      -> widget -- widget       - The child widget to add.
      -> Int    -- leftAttach   - The column number to attach the left 
                --                side of the child widget to.
      -> Int    -- rightAttach  - The column number to attach the right 
                --                side of the child widget to.
      -> Int    -- topAttach    - The row number to attach the top of the 
                --                child widget to.
      -> Int    -- bottomAttach - The row number to attach the bottom of
                --                the child widget to.
      -> IO ()    

    To Adjust individual row or column spacing, call 'tableSetRowSpacing' 
    or 'tableSetColSpacing'; both have the following as a type signature:
    
        TableClass self => self
                        -> Int      - row (or column) number
                        -> Int      - number of pixels spacing should take
                        -> IO ()
                        
    To adjust the spacing for ALL rows, call 'tableSetRowSpacings'
    To adjust the spacing for ALL columns, call 'tableSetColSpacings'.
    The type signature for both is:
            
            TableClass self => self
                            -> Int   - number of pixels spacing should take
                            -> IO ()
    
    To have every cell resize itself to the largest widget in the table,
    call:
        tableSetHomogeneous :: TableClass self
                            => self
                            -> Bool     - True to set all cells to same 
                                          size; False if otherwise
                            -> IO ()
    
    There are also functions for retrieving all these values and the
    size of the table itself:
    
        tableGetRowSpacing :: TableClass self => self -> Int -> IO ()
        tableGetColSpacing :: TableClass self => self -> Int -> IO ()
        tableGetDefaultRowSpacing :: TableClass self => self -> IO ()
        tableGetDefaultColSpacing :: TableClass self => self -> IO ()
        tableGetHomogeneous :: TableClass self => self -> IO Bool
        tableGetSize :: TableClass self => self -> IO (Int, Int)
        
    ----------------------------------------------------------------------
    Other functions defined in Graphics.UI.Gtk.Buttons.Button
    
    buttonNewWithLabel :: string -> IO Button
        create a new Button with a Label child containing the given text
    
    buttonGetLabel :: self -> IO String
        gets the text from the buttons label widget
    
    buttonSetLabel :: self -> string -> IO String
        sets the text of the buttons label widget
        
    [Note: buttons can use 'stock items'; key-value icon/name pairs
           which can be looked up (?) Need to do more digging to
           see how it works.]

    References:
    http://muitovar.com/gtk2hs/chap3-3.html
    https://github.com/PositronicBrain/Gtk2HsTutorial - GtkButton.lhs
    
    To compile:  ghc 03_03_packingTable -o packTable    
-}
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI       -- initialize window
  
  -- create top level window and set attributes
  window  <- windowNew
  set window [windowTitle          := "Packing Table (Grid)", 
              containerBorderWidth := 20,
              windowDefaultWidth   := 150, 
              windowDefaultHeight  := 100]
              
  {-   
    Create a new table (grid) layout with 2 rows and 2 columns
    'True' means resize all cells to size of the largest widget
    if 'False', row will sized to the the height of the largest
                widget in the row
                columns will be sized to the widest widget in the 
                column

  -}  
  -- create a table layout and add it to the window
  table   <- tableNew 2 2 True      -- two rows, two columns
                                    -- resize cells to largest widget
  containerAdd window table
  
  -- place widgets in the table; there are numerous options
  button1 <- buttonNewWithLabel "On"
  tableAttachDefaults table button1 0 1 0 1
  
  button2 <- buttonNewWithLabel "Off"  
  tableAttachDefaults table button2 1 2 0 1
  
  button3 <- buttonNewWithLabel "Quit"
  tableAttachDefaults table button3 0 2 1 2
  
  -- register signal handlers
  on button1 buttonActivated (buttonSwitch button1)
  on button2 buttonActivated (buttonSwitch button2)
  on button3 buttonActivated mainQuit
 
  on window objectDestroy mainQuit
  
  -- start application
  widgetShowAll window
  mainGUI

-- button action, switches button text to 'On' or 'Off' depending
-- on current button text
buttonSwitch :: Button -> IO ()
buttonSwitch b = do
  txt <- buttonGetLabel b
  let newtxt = case txt of
                 "Off" -> "On"
                 "On"  -> "Off"
  buttonSetLabel b newtxt