{-  
    Chapter 8 - Regions
    
    Shapes and Regions are 'sets of points on some surface'; for us,
    the 'surface' is the Cartesian plane.

    Reference:
        'The Haskell School of Expression' by Paul Hudak
        http://www.cs.yale.edu/homes/hudak/SOE/index.htm
-}

module Region( Region
             , Coordinate
             , containsS
             , containsR
             , module Shape
             ) where
             
import Shape   

-- NOTE: infix definitions MUST appear at the top of the file
-- operators - both have right associativity
--           - Intersect has the higher precedence
infixr 5 `Union`
infixr 6 `Intersect`
          

-- new data types
data Region = Shape Shape                 -- a new ctor for a Shape
            | Translate Vector     Region -- translated region
            | Scale     Vector     Region -- scaled region
            | Region   `Union`     Region -- union of two regions
            | Region   `Intersect` Region -- intersection of two regions
            | Complement           Region -- inverse of a region
            | Empty                       -- an empty region
    deriving Show

-- type aliases
type Coordinate = (Float, Float)
type Vector     = (Float, Float) 
type Ray        = (Coordinate, Coordinate)  -- a straight line 
  
  
-- test if the given Coordinate falls within the given Shape
containsS :: Shape -> Coordinate -> Bool
(Rectangle s1 s2) `containsS` (x,y) =
    let t1 = s1/2
        t2 = s2/2
    in - t1 <= x && x <= t1 && - t2 <= y && y <= t2
    
(Ellipse r1 r2) `containsS` (x,y) =
    (x/r1)^2 + (y/r2)^2 <= 1
    
(Polygon pts) `containsS` p =
    let leftOfList   = map isLeftOfp (zip pts (tail pts ++ [head pts]))
        isLeftOfp p' = isLeftOf p p'
    in and leftOfList
    
(RtTriangle s1 s2) `containsS` p =
    (Polygon [(0,0), (s1,0), (0,s2)]) `containsS` p
    
-- if all points in a polygon are on the left the sides
-- then the polygon is convex    
isLeftOf :: Coordinate -> Ray -> Bool
(px,py) `isLeftOf` ((ax,ay),(bx,by)) =
    let (s,t) = (px - ax, py - ay)
        (u,v) = (px - bx, py - by)
    in s * v >= t * u
    
 
    


containsR = undefined