-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 18 - Interactive Keyboard Input and Screen Output

{-
    By convention, Haskell scripts handling IO define a
    'main' variable in their main module
-}
main =
    do  -- following commands in top-down order
    
        -- display a message on the screen
        putStr "Please enter your name.\n"
        
        -- wait for a line of input
        name <- getLine                    

        -- display a message
        putStr("Thank you, " ++ name ++ ".\n" ++
               "Have a nice day (:-)\n" )
               
main' =
    do
        -- get two pieces of input
        putStr "Please enter your name: "
        name <- getLine
        
        putStr "And your email address, please: "
        address <- getLine
        
        putStr(unlines[
            "Thank you, " ++ name ++ ".",
            "I'll send your email to " ++ address,
            "Press Enter to sign off."])
        
        signOff <- getLine      -- wait for Enter
        return()                -- discards entries and quits
        
