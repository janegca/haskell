-- three ways to declare lists
lst   = 1 : 2 : 3 : 4 : []
lst'  = [1,2,3,4]
lst'' = 1 : [2,3,4]

-- concatenating (appending) lists
foo  = [1,2]   ++ [3,4]
foo' = [1,2,3] ++ [4]

-- creating potentially infinite lists 
inf_ones   = 1 : inf_ones       -- using recursion
inf_ones'  = [1,1..]            -- using pattern
inf_ones'' = [1] ++ inf_ones    -- using recursion

-- getting the first element and the rest of a list
first lst = head lst
rest lst  = tail lst

-- getting an element based on index position, zero-based
nth_index n lst = lst !! n
last' lst       = lst !! (length lst - 1)

-- take the first 10 elements of a list, or take everything
-- after the first 10
ten_elems lst         = take 10 lst
eleven_and_beyond lst = drop 10 lst

{-
    Example output:
    
        *Main> lst
        [1,2,3,4]
        *Main> lst'
        [1,2,3,4]
        *Main> lst''
        [1,2,3,4]

        *Main> foo
        [1,2,3,4]
        *Main> foo'
        [1,2,3,4]
        
        *Main> take 10 inf_ones
        [1,1,1,1,1,1,1,1,1,1]
        *Main> take 10 inf_ones'
        [1,1,1,1,1,1,1,1,1,1]
        *Main> take 10 inf_ones''
        [1,1,1,1,1,1,1,1,1,1]
        

        *Main> first lst
        1
        *Main> rest lst
        [2,3,4]

        *Main> nth_index 2 lst
        3
        *Main> last' lst
        4

        *Main> ten_elems [1..20]
        [1,2,3,4,5,6,7,8,9,10]
        *Main> eleven_and_beyond [1..20]
        [11,12,13,14,15,16,17,18,19,20]
    
-}