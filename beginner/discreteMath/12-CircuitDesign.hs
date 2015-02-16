-- Chapter 13 - Circuit Design
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
import Stdm

{-
    Haskell can be used to assist in the precise specification of
    circuits, simulation, correctness proofs, and circuit derivations.
    Only one form of circuit is looked at in this chapter, 
    'combinatorial circuits' that don't contain 'flip-flops' (the
    entire field is too large to cover here).
    
    13.1 Boolean Logic Gates
    ------------------------
    Digital circuits are constructed with primitive circuits called
    'logic gates'; here we use
                0   False
                1   True
    
    Inverter    implements a logical not whose truth table is
    
                        a    inv a
                        -----------
                        0      1
                        1      0
                        
    and2        implements a logical 'and'; takes two inputs
                there are also and3, and4, etc. gates
                
    or2         implements inclusive 'or'
    xor         implements exclusive or
    
                a   b    and2 a b   or2 a b  xor a b
                ------------------------------------
                0   0        0         0        0
                0   1        0         1        1
                1   0        0         1        1
                1   1        1         1        0
-}
{-
    13.2 Functional Circuit Specification
    -------------------------------------
    Since a circuits output is fully dependent on its input we
    can use mathematical functions to model their behaviour.
    
    Example:
        x = and2 a b            -- output of and and2 gate is x
        y = inv (and2 a b)      -- y is the output of the inverted
                                -- result of an and2 gate
        
    The type of a circuit indicates what its inputs and outputs
    are; values carried by wires are called 'signals'. A Signal
    class and the gates are defined in Stdm
    
        class Signal a where
            inv ::  a -> a
            and2, or2, xor :: a -> a -> a
        
    Every type in the Signal class has the constants 'zero' and 'one'
    for False and True; there are also corresponding specialized
    constants False and True.
    
    Circuit Simulation
    ------------------
    It's much cheaper to test circuit design with programmes than
    building and then testing the circuit.
    
    For small circuits, the specification is commonly expressed in the 
    form of a truth table, and you need to design a circuit which 
    implements that truth table.
    
    Every truth table can be written in a general form with one line
    for every possible input combination and a variable p,q,r,s,...
    to specify the output of the combination. For example, the
    general form for a gate that takes to inputs
    
                x  y   f x y
                ------------
                0  0     p
                0  1     q
                1  0     r
                1  1     s
                
    A table with 'k' inputs will have a table of 2^k lines. For
    example, a table with 3-inputs will have a table similar to
    
                a   b   c   f a b c     Concept:
                -------------------     --------  
                0   0   0      0      
                0   0   1      1        The output of f a b c
                0   1   0      0        should be a 1 only when
                0   1   1      1        the inputs (on the left)
                1   0   0      1        correspond to a 1 on the
                1   0   1      0        rightmost column.
                1   1   0      1
                1   1   1      0
                
    All we need is a logical expression that is True for every
    input line that should be true and an expression that combines
    them with a logical or:
        f a b c = expr1 \/ expr2 \/ expr3 \/ expr4
        
    Next, we need to figure out what the expressions are:
    
        expr1 = (not a) /\ (not b) /\ c
        expr2 = (not a) /\ b /\ c
        expr3 = a /\ (not b) /\ (not c)
        expr4 = a /\ b /\ (not c)
        
    This method is not always the most efficient but it is the
    simplest; best to take this approach and transform it to a
    more efficient one once we know everything is working correctly.
    
    Transformation involves a logical proof that two circuits
    implement exactly the same function.
                    
        -- truth table for 'and2' gate
        and2 False False    -- False
        and2 False True     -- False
        and2 True  False    -- False
        and2 True  True     -- True
-}

{-
    Modern circuits can be very large and complex, to large to
    design in one diagram; the key is 'abstraction'; the 
    circuit is organized in a series of levels of abstraction.
    The lowest level has logic gates and other primitive circuits,
    these are used to design the next level up, which can include
    multiplexers, demultiplexors, half and full adders, etc. The
    next level contains basic circuits made up components from
    the previous level and so on.
    
    Multiplexers
    ------------
    The hardware equivalent of an if-then-else
    Takes a control input, a, and two data input: x, y
    There is one output, x if a is 0, otherwise y
    
    Demultiplexer
    -------------
    Opposite of a multiplexor
    Has a control input, a, and a single data input, x
    Produces two outputs: (z1,z2)
    'x' is sent to whichever output 'a' selects, the other
    output is 0.
    

-}
-- multiplexer
mux1 :: Signal a => a -> a -> a -> a
mux1 a x y = or2 (and2 (inv a) x) (and2 a y)

-- demultiplexer
demux1 :: Signal a => a -> a -> (a,a)
demux1 a x = (and2 (inv a) x, and2 a x)
{-
    Bit Arithmetic
    --------------
    The most basic addition circuit, 'half adder', takes two bits 
    (binary digits) and adds them together to produce a two bit
    ouput (c,s) where 'c' is the 'carry' and 's' is the 'sum'.
    The following is a 'half adder' truth table:
    
            a  b   c  s
            ----   ----
            0  0   0  0        Can see that the 'carry' column
            0  1   0  1        matches the truth table for 'and2'
            1  0   0  1        while the 'sum' column matches the
            1  1   1  0        the 'xor' table.
            
    Which means we can implement a 'half adder' with just two
    logic gates.

    Implemented in Stdm
    
        halfAdd :: Signal a => a -> a -> (a,a)
        halfAdd a b = (and2 a b, xor a b)
-}

{-
    It is fairly easy to write full tests for small circuits
    but with larger ones that is not always possible; separate
    proofs are needed. We also want to prove 'correctness'.
    To help in this we can define a 'bitValue' that converts
    a bit signal to an integer.
    
    Theorem 106
    
        Let (c,s) = halfAdd a b. Then
            
            2 * bitValue c + bitValue s = bitValue a + bitValue b
            
-}
-- NOTE: texts refers to a 'Static' class to ensure fixed values
--       but they didn't include the code; using False and True
--       directly works ok

bitValue :: Bool -> Int
bitValue x = if x == False then 0 else 1

{-
    In order to add 'words' representing binary numbers 
    we need to add three bits, one for each words and a
    carry bit; again, we output two values (c', s)
    
                a   b   c  c' s
                ---------  ----
                0   0   0  0  0
                0   0   1  0  1
                0   1   0  0  1
                0   1   1  1  0
                1   0   0  0  1
                1   0   1  1  0
                1   1   0  1  0
                1   1   1  1  1

    While there are many ways of implementing a 'full adder',
    traditionally it is implemented with two 'half adders'
    
    Theorem 107 (Correctness of full adder)
        Let (c',s) = fullAdd(a,b) c so that c' is the carry
        output and s is the sum output. Then
        
        bitValue c' x 2 + bitValue s = bitValue a + bitValue b 
                                     + bitValue c
                                     
        
-}
fullAdd' :: Signal a => (a,a) -> a -> (a,a)
fullAdd' (a,b) c = (or2 w y, s)
    where (w,x) = halfAdd a b
          (y,s) = halfAdd x c   
          
{-
    Binary Representation
    ---------------------
    Binary numbers consist of a sequence of bits called a 'word'.
    We can represent a 'word' as a list.  There are two schemes
    for numbering the list elements in a word: [x0,x1,x2,x3] or
    [x3,x2,x1,x0]. We will use the first, [x0,x1,x2,x3] where
    'x0' is the leftmost bit and x3 is the rightmost with index (k-1)
    and 'k' is the number of bits in the word.
    
    The value of the word [x0,x1,x2,x3] is:
    
        x0 * 2^3 + x1 * 2^2 + x2 * 2^1 + x4 * 2^0
        
    The smallest value that can be represented in binary is 0
    The largest is 2^(k-1)
      
-}          
wordValue :: [Bool] -> Int
wordValue []     = 0
wordValue (x:xs) = 2^k * bitValue x + wordValue xs
    where k = length xs
    
{-
    13.3 Ripple Carry Addition
    --------------------------
    A 'ripple carry' adder is used to calculate the sum of two
    words. It also has an additional 'carry' input that allows
    it add numbers larger than 4-bits by performing a sequence
    of additions.
    
    The output is a single carry bit and a word of sum bits
    The two input words and the output word but have the same
    number of bits.
    
    add4 (below) is a 4-bit ripple carry adder that uses 
    four full adders.
    
    Example:
        To add 3 + 8
        (a) convert binary 3 and to True,False equivalents
        (b) group in corresponding pairs
        (c) evaluate
        
        3 = 0011 -> [False, False, True,  True]
        8 = 1000 -> [True,  False, False, False]
        add4 False [(False,True), (False,False), (True,False),
                     (True,False)]
        -> (False, [True,False,True,True]) 
-}    

add4' :: Signal a => a -> [(a,a)] -> (a,[a])
add4' c [(x0,y0),(x1,y1),(x2,y2),(x3,y3)] =
    (c0, [s0,s1,s2,s3])
    where 
        (c0,s0) = fullAdd' (x0,y0) c1
        (c1,s1) = fullAdd' (x1,y1) c2
        (c2,s2) = fullAdd' (x2,y2) c3
        (c3,s3) = fullAdd' (x3,y3) c    

{-
    Circuit Patterns
    ----------------
    add4 is not too complicated but it would be tedious to expand
    it to cover 32 and 64 bit words (standard word sizes in computing)
    
    It would be much better to define a general k-bit ripple carry
    adder that would work for any word size.
    The most intuitive description of the adder would say 'each full 
    adder has its carry input connected to the carry output of its 
    right neighbour'
    
    The 'manscr' function (defined in Stdm) is a 'black-box'
    that takes a 'building block' and makes as many copies
    as necessary along with all the required internal 
    connections.
    
    
        mscanr :: (b->a->(a,c)) -> a -> [b] -> (a,[c])
        mscanr f a [] = (a,[])
        mscanr f a (x:xs) =
            let (a’,ys) = mscanr f a xs
                (a’’,y) = f x a’
            in (a’’, y:ys)    
            
            
   The 'mscanr' pattern
        
              x0   x1   x2         x(n-1)
              |    |    |          |
        a' <- f <- f <- f . . . <- f <- a
              |    |    |          | 
              y0   y1   y2         y(n-1)
              
        - a row of 'full adders' in every bit position
          the 'carry' input to each adder is connected to the
          carry output from the full adder to its right
          and the carry input to the right-most (least
          significant) bit position is the carry input to
          the entire addition
              
    The 'black-box' for 'mscanr' pattern:
          
                          ::[b]
                            |          
                :: a <-  mscanr f  <- :: a
                            |
                          ::[c]
                          
    The n-bit Ripple Carry Adder
    ----------------------------
    We can use 'mscanr' to build a general ripple carry adder
    that will work for any word size.
    
    The first argument to 'mscanr' is a circuit specification
    with type: b -> a -> (a,c) which fits with the type signature
    of a full adder:  Signal a => (a,a) -> a -> (a,a)
    
    So rippleAdd is defined as:
    
        rippleAdd :: Signal a => a -> [(a,a)] -> (a, [a])
        rippleAdd c zs = mscanr fullAdd c zs
        
    Example:
        use rippleAdd to ad 23 + 11
        
        23 -> 010111 [False, True,  False, True,  True, True]
        11 -> 001011 [False, False, True,  False, True, True]
        
        rippleAdd False [(False,False), (True,False), (False,True),
                         (True, False), (True, True), (True, True)]
        -> (False, [True,False,False,False,True,False])
        
    Theorem 108
        Let xs and ys be k-bit words, so xs, ys :: Signal [a] -> a
        Define (c, sum) = rippleAdc zero (zip xs ys), thus c :: a
        is the carry output and ss::[a] is the sum word. Then,
        
        bitValue c * 2^k + wordValue ss = wordValue xs + wordValue ys
        
        where the left-hand side is the numeric value of the output
              and the right-hand side is the numeric value of the
              input
        
-}
{-
    Binary Comparisons
    ------------------
    First we need a function that compares two bits, returning
    a triple whose bits correspond to (lt,eq,gt) (see halfCmp' below
    which is analogous to halfAdd) and then a fullCmp (which is 
    analogous to a fullAdd) and then finally we can create a
    rippleCmp (analogous to rippleAdd)
    
    
-}
halfCmp :: Signal a => (a,a) -> (a,a,a)
halfCmp (x,y) =
    (and2 (inv x) y,    -- x<y when x=0,y=1
    inv (xor x y),      -- x=y when x=0,y=0 or x=1,y=1
    and2 x (inv y))     -- x>y when x=1,y=0
        
fullCmp :: Signal a => (a,a,a) -> (a,a) -> (a,a,a)
fullCmp (lt,eq,gt) (x,y) =
    (or2 lt (and3 eq (inv x) y), -- <
    and2 eq (inv (xor x y)),     -- =
    or2 gt (and3 eq x (inv y)))  -- >        
    
-- assume the two are equal, and compare, reading left to right
rippleCmp :: Signal a => [(a,a)] -> (a,a,a)
rippleCmp z = foldl fullCmp (False,True,False) z                     