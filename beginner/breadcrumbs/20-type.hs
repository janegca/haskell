-- every function has a type
add :: Int -> Int -> Int
add a b = a + b

-- in general, this is more like the following (not valid haskell code)
--      foo :: InputType -> OutputType

-- or for a function of five arguments (as an example)
--      bar :: InputType1 -> InputType2 -> InputType3 -> InputType4 
--          -> InputType5 -> OutputType

-- or a function of no arguments!
--      baz :: OutputType

-- for instance
name = "Joe Schmoe" -- this is a function! really!
