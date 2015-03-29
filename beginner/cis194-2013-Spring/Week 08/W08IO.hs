{-
    Week 08 - IO
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
-} 
{-
    The problem with purity
    -----------------------
        1. Functions may not have external (side) effects.
        2. Functions may not depend on externals (like the
           mathematical notion of a function; given the
           same input, must always produce the same output).
           
    But we often need our programs to interact with external stuff;
    the solution:
    
    The IO type
    -----------
    IO types describe computations that may or may not interact
    with externals and return, or not return, a value.  Such a
    type has 'no value' until it is executed.
    
    i.e. best to think of them as 'a recipe for a value' rather
         than a value in and of themselves; much like the difference
         between a cake and a cake recipe
         
    For example, there is no 'String' in IO String -- you cannot
    get a String out of it. IO String is an action that, when
    executed, can produce a String.
    
    And the only way to 'execute' an action is within an IO context,
    or 'main' action which gets passed to the runtime system.
    
    [Note that WinGhci and Ghci are IO Monads with a 'main' action
     and everything you enter gets processed through that context.
     if you compile a module without a name, it is given the name Main 
     by default. If a module name is used, and a main function exists,
     you can compile using:
        ghc -main-is ModuleName -o AppName fileName.hs
        
        where -main-is tells the compiler which file is to be considered
                       the equivalend of module Main
                       
              -o  tells the compiler what to name the .exe file
      
     see TestMain.hs]
     
     
    Combining IO
    ------------
        There are two sequence operators: (>>) and (>>=)
        
        (>>) 'and then' - executes two IO actions in sequence
        (>>=) 'bind'    - executes two IO actions in sequence,
                          passing the result of the first to the second
-}
{-
    Record Syntax
    -------------
    
    Data can be defined with field names:
    
        data D = C {fn1 :: T1, fn2 :: T2, fn3 :: T3}
        
    Benefits to doing so:
    
    1. Each field name is automatically a 'projection' function
        fn2 :: D -> T2      == fn2 (C _ f _ ) = f
        
    2. We can construct a value of type D as
            C {fn3 = ..., fn1 = ..., fn2 = ...}
       ie field order is not important
       
       And we can modify a value d using:
            d {fn1 = ...}
            
       which creates a new 'd' value from the old 'd' value with
       the value of fn1 changed.
       
       And we can pattern match
            foo (C {fn1 = x}) = x
       which matches solely on fn1, calling its value, x
            
-}