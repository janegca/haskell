{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
{-
    Compile: 
        ghc --make DiagramsTutorial.hs
        
    Run:
        DiagramsTutorial -o circle.svg -w 400
    
    Creates a circle.svg file that can be viewed in a browser

    To see all the available options enter (on the command-line)
        DiagramsTutorial --help
-}

main = mainWith (circle 1 :: Diagram B)