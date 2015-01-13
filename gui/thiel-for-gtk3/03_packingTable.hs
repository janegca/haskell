--
-- References:
--   http://muitovar.com/gtk2hs/chap3-3.html
--   https://github.com/PositronicBrain/Gtk2HsTutorial - GtkButton.lhs
--
-- To compile:  ghc 03_packingTable -o packTable
--
{-
    When adding a widget to a table layout, use 'tableAttach' or
    'tableAttachDefaults', both of which have a number of options
    
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
-}
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
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
    sizing layout starts at top,left 
    
             0          1          2
            0+----------+----------+
             |          |          |
            1+----------+----------+
             |          |          |
            2+----------+----------+    

  -}  
  table   <- tableNew 2 2 True
  containerAdd window table
  
  -- place widgets in the table; there are numerous options
  button1 <- buttonNewWithLabel "On"
  on button1 buttonActivated (buttonSwitch button1)
  tableAttachDefaults table button1 0 1 0 1
  
  button2 <- buttonNewWithLabel "Off"
  on button2 buttonActivated (buttonSwitch button2)
  tableAttachDefaults table button2 1 2 0 1
  
  button3 <- buttonNewWithLabel "Quit"
  on button3 buttonActivated mainQuit
  tableAttachDefaults table button3 0 2 1 2
  
  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI

-- button action  
buttonSwitch :: Button -> IO ()
buttonSwitch b = do
  txt <- buttonGetLabel b
  let newtxt = case txt of
                 "Off" -> "On"
                 "On"  -> "Off"
  buttonSetLabel b newtxt