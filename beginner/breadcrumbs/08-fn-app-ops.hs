-- Function Application Operator
--  ($) - has the lowest precedence of any operator
--        any function to the right of the '$' will
--        be executed last

-- the name is scary, the actual thing is not

add = (+)  -- (+) is the way to refer to the + as a regular 
           -- (not special & infix) function
           
divide = (/)

foo  = add 2 (divide 4 5)
foo' = add 2 $ divide 4 5

{-
    the idea is that 
        blah $ foo bar baz 
        
    is shorthand for 
        blah (foo bar baz)
        
    it's a way to save parenthesis :-)
-}
{-
    Example:
    
    [1 of 1] Compiling Main         ( 08-fn-app-ops.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> add 1 2
    3.0
    *Main> 1 `add` 2
    3.0
   
    *Main> divide 6 2
    3.0
    *Main> 6 `divide` 2
    3.0
    
    *Main> foo
    2.8
    *Main> foo'
    2.8
    *Main>    
    
-}