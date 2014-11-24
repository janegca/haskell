{-
    Exercise: Towers of Hanoi
    Source:
        CIS 194: Introduction to Haskell (Fall 2014), Richard Eisenberg
        Week 1 Homework http://www.seas.upenn.edu/~cis194/lectures.html
        
    The Towers of Hanoi is a classic puzzle with a solution
    that can be described recursively. Disks of different sizes are stacked
    on three pegs; the goal is to get from a starting configuration with
    all disks stacked on the first peg to an ending configuration with all
    disks stacked on the last peg.
    
    The only rules are
    • you may only move one disk at a time, and
    • a larger disk may never be stacked on top of a smaller one.  

    For example, as the first move all you can do is move the topmost,
    smallest disk onto a different peg, since only one disk may be moved
    at a time.
    
    From this point, it is illegal to move the next disk on the first
    peg to the disc you've just placed on the second peg as the next
    disc is larger than the first.
    
    To move n discs (stacked in increasing size) from peg a to peg b
    using peg c as temporary storage,
    
    1. move n - 1 discs from a to c using b as temporary storage
    2. move the top disc from a to b
    3. move n - 1 discs from c to b using a as temporary storage.  
    
    Given the number of discs and names for the three pegs, hanoi
    should return a list of moves to be performed to move the stack of
    discs from the first peg to the second.    
    
    Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")] 

    Number of steps required to move discs given by:
        2^(# of discs) - 1
    i.e. 2^2 - 1 = 3
         2^5 - 1 = 31
    
-}
type Peg = String           -- type synonym
type Move = (Peg, Peg)      -- new type for pair of pegs
                            -- representing one 'move'

-- pegs variables are 's'ource, 'd'estination and 't'emporary                            
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n s d t = hanoi (n-1) s t d 
              ++ ((s,d) : hanoi (n-1) t d s)
              
-- number of moves required to transfer given number of discs
hanoiNumSteps :: Integer -> Int
hanoiNumSteps n = length (hanoi n "a" "b" "c")

{-
    Optional
    
    What if there are 4 pegs instead of 3?
    Write a function similar to hanoi which solves this problem
    in as few moves as possible. It should be possible to move
    the discs to their final destination in far fewer steps than
    were required when using three pegs.
    i.e. with 3 pegs, 2^15 - 1 = 32767 moves; with 4 pegs should
         only take 129 moves (2^k + 1 = 2^7 + 1)
-}
                  
-- Ref: Robert J. Swartz 'Multipeg Towers of Hanoi' Applet
--      http://mathapplets.net/ helped with visualizing the
--      problem
--
--      Michael Rand 'On the Frame-Stewart Algorithm'
--      http://www.math.columbia.edu/~jason/Rand.thesis3.pdf
--      formula for optimal disc partition for 4 peg problem
--          n - sqrt (2*n) + 0.5
--      Note that a k-peg solution requires the building of Pascal
--      Triangles and lookups (a later TODO)
--
-- Frame-Stewart Algorithm
--      move top partition of discs to spare peg using 4 pegs
--      move bottom partition of discs to destination peg using hanoi
--          3 peg solution
--      move top partition of discs to destination disc using 4 pegs
--
-- Pegs are 's'ource, 'd'estination, with t1 and t2 for temporary
-- storage
--
-- Examples:
--      hanoi'  3 "a" "b" "c" "d"  -> list of   5 moves
--      hanoi'  5 "a" "b" "c" "d"  -> list of  13 moves
--      hanoi' 15 "a" "b" "c" "d"  -> list of 129 moves
--      hanoi' 16 "a" "b" "c" "d"  -> list of 161 moves
--
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ _   = []
hanoi' 1 s d _ _   = [(s,d)]
hanoi' 2 s d t1 _  = [ (s,t1), (s,d), (t1, d)]
hanoi' 3 s d t1 t2 = [ (s,t1), (s,t2), (s,d), (t2,d), (t1,d) ]
hanoi' n s d t1 t2 =  hanoi' top s t2 t1 d     -- top discs on t2 peg
                   ++ hanoi  bot s d t1        -- bottom discs to d peg
                   ++ hanoi' top t2 d s t1     -- top discs to d peg
    where x   = fromIntegral(n) :: Double
          top = floor(x - sqrt (2*x) + 0.5)
          bot = n - top
    
    
                   
