-- Ref: Two Dozen Short Lessions in Haskell by Rex Page
-- Chapter 5 Function Composition

{-
    The use of more than one function in a formula is known as
    'function composition'.  
    
    Function composition applies a succession of transformations
    to supplied data and is the most common pattern of computation.
    
    Below three functions are combined to produce a result
-}

removePeriods :: String -> String
removePeriods str = [c | c <- str, c /= '.']

removeBlanks :: String -> String
removeBlanks  str = [c | c <- str, c /= ' ']

removeCommas :: String -> String
removeCommas  str = [c | c <- str, c /= ',']

madam = "Madam, I'm Adam."
result = removeCommas( removePeriods( removeBlanks madam))

{-
    Order of evaluation is:
        removeCommas(removePeriods(removeBlanks madam))
        => removeCommas(removePeriods(removeBlanks "Madam, I'm Adam."))
        => removeCommas(removePeriods "Madam,I'mAdam.")
        => removeCommas "Madam,I'mAdam"
        => "MadamI'mAdam"
        
    The 'dot' operator (.) allows us to remove parentheses and
    rewrite the formula as show below (result1); producing exactly
    the same output as 'result'
-}
result1 = (removeCommas . removePeriods . removeBlanks) madam
sameResult = result == result1

{-
    The dot operator allows for the following:
    
        (f . g) x     == f(g (x))
        (f . g . h) x == f(g(h(x)))
        
        and so on
    
    Which point to a way in which we can 'parameterize' a function
    to remove periods, commas and blanks from any string
-}
removeBPC str = (removeCommas . removePeriods . removeBlanks) str

{-
    Note that in a formula like (f . g . h) x, the 'f . g. h' is
    a function in it's own right that, when applied to the argument
    'x', produces a value: (f . g .h) x
    
    For example, 'reverse' is a function
                 reverse "Chicago" is the VALUE "ogacihC"
                 
-}
{-
    Note that the pattern of the 3 functions is exactly the same
    except for the character in the 'guard'
        c /= ' '
        c /= ','
        c /- '.'
    We can generalize the 3 into a single function, 'remove' that takes
    two arguments: a char and a string. We could then remove any
    character (or series of characters) that we choose.
    
    Example:
    
        *Main> remove ' ' "Madam, I'm Adam."
        "Madam,I'mAdam."
        *Main> remove '.' it
        "Madam,I'mAdam"
        *Main> remove ',' it
        "MadamI'mAdam"    
    
    Note: 'it' is a ghci variable that holds the result of the last
          action called
-}
remove chr str = [c | c <- str, c /= chr]

{-
    We can still use the dot operator to create a sequence
    of character removals
-}
result2 = (remove '.' . remove ' ' . remove ',') madam
sameResult1 = (result == result1) && (result1 == result2)  -- True

{-
    When we invoke a function with fewer than all its arguments
    it's called a 'curried invocation'
    
    ie the 'remove' function has 2 arguments: chr str
       when we invoke it as: remove '.' we supply a value, '.',
       for the first argument: chr, but we defer the assignment
       of the second argument, str.
       
       In a sense, when we write: remove '.'
       we are creating a new function that is a specialezed version
       of 'remove' ie we are creating a 'remove period' function
       just like our original 'removePeriods' function
       
            removePeriods -> [c | c <- str, c /= '.']
            remove '.'    -> [c | c <- str, c /= '.']
            
        both functions do exactly the same thing, (they must do
        as they both have the same formulas] and, in fact,
        we can assign: remove '.' to a named function, 'removePer'
        and all 3 versions will produce the same result
-}

removePer = remove '.'

{-        
        Example:
        
            *Main> removePeriods "a.b.c"
            "abc"
            *Main> remove '.' "a.b.c"
            "abc"
            *Main> removePer "a.b.c"
            "abc"        
-}

-- further examples
f str = [c | c <- str, c == 'x']
g str = [c | c <- reverse str, c < 'n']
teddy = "A man, a plan, a canal. Panama!"

-- what output do you expect from?
res1 = (f . g) teddy == f(g teddy)

-- what if the guards in f and g are reversed?
f1 str = [c | c <- str, c /= 'x']
g1 str = [c | c <- reverse str, c > 'n']

res2 = (f1 . g1) teddy == f1(g1 teddy)

{-
    There is also a 'function application' operator ($)
    that allows for the removeal of the parentheses around
    (f1 . g1)
    
    The $ operator says take everything to the left of me
    and assign it to whate is on my right so
    
        f1. g1 $ teddy == (f1 . g1) teddy
-}

res3 = f1 . g1 $ teddy
res4 = res3 == (f1 . g1) teddy
