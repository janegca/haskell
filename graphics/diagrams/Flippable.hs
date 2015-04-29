{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

{-
    Compile: ghc --make Flippable.hs
    Run:     Flippable -o Flippable.svg -w 400
             Flippable -0 Flipped.svg -w 400 --flipped
             
    View results in a browser.

-}

import           Diagrams.Backend.CmdLine
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude  hiding ((<>))
import           Options.Applicative

data FlipOpts = FlipOpts Bool

instance Parseable FlipOpts where
  parser = FlipOpts <$> switch (long "flipped" 
                    <> help "Flip the diagram L-R")
 
d :: Diagram B  --SVG V2 Double
d = square 1 # fc red ||| square 1 # fc blue      

main = mainWith (\(FlipOpts f) -> (if f then reflectX else id) d)

  