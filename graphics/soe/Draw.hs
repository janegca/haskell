{-
    Chapter 4 - Drawing Shapes
    
    Reference:
        'The Haskell School of Expression' by Paul Hudak
        http://www.cs.yale.edu/homes/hudak/SOE/index.htm
-}
module Draw( inchToPixel
           , pixelToInch
           , intToFloat
           , xWin
           , yWin
           , trans
           , shapeToGraphic
           , withColor
           , spaceClose
           ) where

import Shape
import SOE

-- assume x is in inches and there are 100 pixels per inch
inchToPixel :: Float -> Int
inchToPixel x = round (100 * x)

pixelToInch :: Int -> Float
pixelToInch n = intToFloat n / 100

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

-- define global names for our window size
xWin, yWin :: Int
xWin = 600
yWin = 500

-- graphics coord system has (0,0) for upper left corner
-- we normally use (0,0) as the centre of an (x,y) axis
-- this translates our Cartesian coords into window coords
-- so, trans(0,0) => (xWin2, yWin2)
trans :: Vertex -> Point
trans (x,y) = (xWin2 + inchToPixel x,
               yWin2 - inchToPixel y)
               
xWin2, yWin2 :: Int
xWin2 = div xWin 2
yWin2 = div yWin 2

-- translates a list of vertices into a list of points
-- required by a graphics window
transList :: [Vertex] -> [Point]
transList []     = []
transList (p:ps) = trans p : transList ps         

-- convert a Shape to a Graphic that can be drawn using 'draw'
shapeToGraphic :: Shape -> Graphic
shapeToGraphic (Ellipse r1 r2) = ellipse(trans(-r1,-r2)) (trans(r1,r2))
shapeToGraphic (RtTriangle s1 s2) = polygon(transList[(0,0),(s1,0),(0,s2)])
shapeToGraphic (Polygon vts) = polygon(transList vts)
shapeToGraphic (Rectangle s1 s2) 
    = let s12 = s1/2
          s22 = s2/2
      in polygon(transList[(-s12,-s22),(-s12,s22),(s12,s22),(s12,-s22)])
                  
-- loop until user presses 'space' key, then close window   
spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w
      
-- Examples of Shapes to Graphics
sh1, sh2, sh3, sh4 :: Shape
sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon[(-2.5,2.5),(-1.5,2.0), (-1.1,0.2),
              (-1.7,-1.0), (-3.0,0)]

main0 = runGraphics(
    do w <- openWindow "Drawing Shapes" (xWin, yWin)
       drawInWindow w (withColor Red (shapeToGraphic sh1))
       drawInWindow w (withColor Blue (shapeToGraphic sh2))
       spaceClose w
    )

-- define a function to draw a whole list of shapes
type ColoredShapes = [(Color,Shape)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w [] = return ()
drawShapes w ((c,s):cs) = 
    do drawInWindow w (withColor c (shapeToGraphic s))
       drawShapes w cs
       
main1 = runGraphics(
    do w <- openWindow "Drawing Shapes" (xWin, yWin)
       drawShapes w shs
       spaceClose w
    )
    
    
shs :: ColoredShapes
shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]

                      