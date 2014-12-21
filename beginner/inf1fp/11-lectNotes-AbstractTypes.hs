-- Informatics 1 - Functional Programming 
-- Lecture 11 - Abstract Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  3/11/2014  26:00 minute mark
--         4/11/2014  full video
--        10/11/2014  13:00 minute to 33 minutes
-- Ref:
--   for O-notation:
--      http://www.daveperrett.com/articles/2010/12/07
--      /comp-sci-101-big-o-notation/

{-
    Complexity
        analyzing how fast things run
        remember that clarity is more important; make sure your
        programs work BEFORE attempting any optimization
        
    Abstract Types
        "a way of making things clear before making them fast"
        write you program at high level of representation
        
    We'll look at different ways of looking at sets of elements
    
    COMPUTING TIME
    
    A set can be represented as a list
        Given a list of length n, how long does it take to check
        if an element is in the list?  Depends on the length of the
        list, n, on average: n/2
        
    n   - linear time    - straight line  - linear is faster for any n > 1
    n^2 - quadratic time - parabolic
    
    a linear program is always faster than a quadratic program for
    large data sizes
    
    eyeball the complexity of your program to work out whether its
    linear (recursive doing a set number of things) or quadratic (
    recursive calling another piece of code that is linear)
    
    O-NOTATION (think of 'O' as 'order')
        O(1)    - constant time  - regardless of size of input,
                                   operation takes the same amount
                                   of time
        O(n)    - linear         - depends on n, bigger it is, longer
                                   it takes        
        O(n^2)  - quadratic      - for every input you are doing 
                                   something to every other input
                                   every time n doubles, the operation
                                   takes 4 times longer
        O(2^n)  - exponential    - every added element doubles the
                                   processing time
        O(log n) - logarithmic   - can double the input and it only
                                   takes 1 unit of extra processing time
        O(n log n) - logarithmic - most efficient, you are performing
                                   O(log n) operations for every n
        
        a way of talking about complexity by focusing on the shape
        of the curves
        
        the steeper the curve, the longer the time (time on y-axis)
        
-}
{-
    Representations of sets have varying levels of complexity
    
    Sets as                                      See file
        unordered list without abstraction       ListsUnabs.hs
        ordered list without abstraction         OrderedListUnabs.hs
        ordered trees without abstraction        TreeUnabs.hs
        balanced tree without abstraction        BalancedTreeUnabs.hs
    
    Key to avoid breaking the abstraction is:
        (1) provide constructors for your data types, and
        (2) keep the constructors hidden (don't export them)
        
    Following files add abstraction to each of the types discussed
    above:
        list with abstraction                    ListAbs.hs
        ordered list with abstraction            OrderedListAbs.hs
        ordered trees with abstraction           TreeAbs.hs
        balanced tree with abstraction           BalancedTreeAbs.hs
        
-}
