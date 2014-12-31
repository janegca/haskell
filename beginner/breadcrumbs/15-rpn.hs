-- RPN (Reverse Polish Notation) 
--      simulates a calculator
-- ignore the parts that haven't been explained yet, and come back 
-- to it later
-- the things to ignore are noted

rpn :: String -> [Int] -- ignore this
rpn str = foldr stackSolve [] (reverse (words str)) 
-- you probably don't know what foldr means yet (but you might!)
  where stackSolve :: String -> [Int] -> [Int]
        stackSolve [] _       = []
        stackSolve word stack = case word of
          "+" -> binOp (+) stack
          "-" -> binOp (-) stack
          "*" -> binOp (*) stack
          "/" -> binOp div stack
          "%" -> binOp mod stack
          _   -> (read word) : stack
          
        binOp _ []       = error "Stack underflow (0 items on stack, binOp)"
        binOp _ (x:[])   = error "Stack underflow (1 item on stack, binOp)"
        binOp f (x:y:xs) = ((flip f) x y) : xs   
        -- flip takes a function of two arguments and reverses their order
        
{-
    Example 
    
    *Main> rpn "3 4 +"
    [7]
    *Main> rpn "3 4 + 2 -"
    [5]
    *Main> rpn "3 4 + 2 - 7 *"
    [35]    
    
    Example of what flip does
    
    *Main> (/) 4 2
    2.0
    *Main> (flip (/)) 4 2
    0.5 
-}        