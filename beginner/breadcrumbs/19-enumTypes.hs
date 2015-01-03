data EnumeratedType = Foo | Bar | Baz

func Foo = "Got a foo!"
func Bar = "Got a bar!"
func Baz = "Got a baz!"

func' enum = case enum of
               Foo -> "Got a foo!"
               Bar -> "Got a bar!"
               Baz -> "Got a baz!"

decide n | n < 0     = Foo
         | n == 0    = Bar
         | otherwise = Baz
             
run  = func  . decide
run' = func' . decide

{- example usage in ghci:
run (-1)   -- you need the parens to distinguish the negative number from 
           -- a partial application of the (-) function
run 0
run 1
run 10
-}
{-
    [1 of 1] Compiling Main             ( 19-enumTypes.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> run (-1)
    "Got a foo!"
    *Main> run 0
    "Got a bar!"
    *Main> run 1
    "Got a baz!"
    *Main> run 10
    "Got a baz!"
    *Main> 
-}