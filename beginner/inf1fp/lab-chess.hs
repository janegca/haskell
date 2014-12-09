-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II - Chess
--      Required support files can be found at:
--           http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/Labweek.zip

import PicturesSVG
import Test.QuickCheck

-- Exercise 9:

pic1 :: Picture
pic1 = above (knight `beside` (invert knight))
             ((invert knight) `beside` knight)

pic2 :: Picture
pic2 = above (knight `beside` (invert knight))
             ((flipV (invert knight)) `beside` (flipV knight))

-- Exercise 10:
-- a)
emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

rkb :: Picture
rkb = rook `beside` knight `beside` bishop

qk :: Picture
qk = queen `beside` king

-- need to keep all the knights facing left so can't just flip rkb
bkr :: Picture
bkr = bishop `beside` knight `beside` rook

whiteLineUp :: Picture
whiteLineUp = rkb `beside` qk `beside` bkr

whiteRow :: Picture
whiteRow = over whiteLineUp otherEmptyRow

blackRow :: Picture
blackRow = over (invert whiteLineUp) emptyRow

-- e)
whitePawnLineUp :: Picture
whitePawnLineUp = repeatH 4 (beside pawn pawn)

whitePawns :: Picture
whitePawns = over whitePawnLineUp emptyRow

blackPawns :: Picture
blackPawns = over (invert whitePawnLineUp) otherEmptyRow

populatedBoard :: Picture
populatedBoard = above
                 (above (above blackRow blackPawns) middleBoard)
                 (above whitePawns whiteRow)

-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoBeside(twoAbove x)
