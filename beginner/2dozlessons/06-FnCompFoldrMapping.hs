-- Ref: Two Dozen Short Lessions in Haskell by Rex Page
-- Chapter 6 Fn Composition, Folding and Mapping

import Data.Char (toLower)          -- used in mapping section below

{-
    Folding and mapping are two common patterns (other than function
    composition) found in many programs.
    
    Folding REDUCES a SEQUENCE of values to a SINGLE VALUE by
    combining adjacent pairs of values.
    
    Mapping applies the same transformation to every value in 
    a sequence of values.
-}
{-
    Function Composition (.)
        operator takes 2 arguments: f and g
        f and g are both functions that TRANSFORM arguments of
        a specific type (let's call it 't') into results of the
        same type 't'.
        So, the function: f . g transforms arguments of type 't'
        into results of type 't'
        
        i.e.  (f . g) x == f(g(x))
              since 'g' requires an argument of type 't' and returns
              a value of type 't' which is passed on to 'f', which
              also takes and returns a value of the same type 't'
        
        And the same would hold if we added a third function, h
            (f . g . h) x == f(g(h(x)))
        if f, g and h all take a value of type 't' then 'x' must
        be of type 't' and f . g . h would return a value of type
        't'.  And so on if we added a function j and m and n and
        so on down the line.
              
        FUNCTION COMPOSITION ALLOWS US TO BUILD AN ASSEMBLY LINE
        OPERATION FROM A SEQUENCE OF FUNCTIONS.
        
        Evaluation starts on the RIGHT with the argument being passed
        to the right-most function in the sequence whose result is
        passed to the function directly to it's left who transforms
        the data and passes it to the function on it's left and so
        on until we reach the last function on the left.
        
        i.e. (f . g . h . j . k) x
             => (f . g . h. j) res_kx
             => (f . g . h) res_jkx
             => (f . g) res_hjkx
             => f( res_ghjkx )
             => res_fghjkx
-}
{-
    FOLDING 
        there is an 'intrinsic function': foldr1, which inserts
        an operation between elements of a sequence and folds them
        all together into a single value by applying the operation
        to adjacent pairs (just as the function composition 'folded'
        all the results down to one result)
        
        For example, if 'pre' is a function that returns (from a pair
        of elements) the one that comes first alphabetically
        
        foldr1 pre "waffle"
        => 'w' pre 'a' pre 'f' pre 'f' pre 'l' pre 'e'
        => 'w' pre 'a' pre 'f' pre 'f' pre ('l' pre 'e')
        => 'w' pre 'a' pre 'f' pre ('f' pre 'e')
        => 'w' pre 'a' pre ('f' pre 'e')
        => 'w' pre ('a' pre 'e')
        => 'w' pre 'a'
        => 'a'
        
        foldr1 can be used with the function composition operator
        to build an assembly line of functions
        i.e     
                foldr1 (.) [f, g, h] MEANS  f . g . h
                
        here the dot operator has to be surrounded by parentheses
        to 'package' it up as function to be applied by foldr1
        in other words:
                    f . g == (.) f g
        the 'dot' acts as an 'infix operator' in the first case and as
        a function in the second. In fact, we can do the same with
        standard operators lie +, -, \, =, etc. i.e. we can turn them
        into functions so
        
            1 + 2  == (+) 1 2
            2 - 1  == (-) 2 1
            
        Named functions can be turned into operators by using 
        backticks.
            pre 'a' 'b' == 'a' `pre` 'b'
            div 6 3     ==  6  `div`  3
        
        [f, g, h] is a 'list' or 'sequence' of functions that we
        want to apply the 'dot' operator to. Note that a list can
        hold any elment type: String, Char, Person, etc BUT every
        element in the list MUST HAVE THE SAME TYPE.
        
        In our example, f, g and h are all functions.
        In a string, all the elements are characters
        ie "abc" == ['a','b','c']
        In fact, strings in Haskell are simply lists (sequences)
        of characters.
        
        We could also have written
            foldr1 (.) [remove ',' , remove ' ', remove '.']
            
        which is equivalent to: remove ',' . remove ' ' . remove '.'
        
       

-}
madam = "Madam, I'm Adam."
remove char str = [c | c <- str, c /= char]
removeBPC str = (remove ',' . remove ' ' . remove '.') str

foldRemove str = foldr1 (.) [remove ',', remove ' ', remove '.'] str

res1 = removeBPC madam
res2 = foldRemove madam
res1EqRes2 = res1 == res2

{-
    So   foldRemove "Madam, I'm Adam." 
    evaluates roughly as follows:
    
         foldr1 (.) [remove ',', remove ' ', remove '.'] "Madam, I'm Adam."
     ==> foldr1 (.) [remove ',', remove ' '](remove '.' "Madam, I'm Adam.")
     ==> foldr1 (.) [remove ','](remove ' ' "Madam, I'm Adam")
     ==> foldr1 (.) (remove ',' "Madam,I'mAdam")
     ==> foldr1 (.) "MadamI'mAdam"
     ==> "MadamI'mAdam"
-}

{-
    MAPPING
    
    A mapping transforms all the elements in a list (sequence) by
    applying a function i.e. mapping creates a new sequence from
    an old by applying a function to each of the elements
    
        [f x | x <- xs]  maps the function 'f' onto the sequence 'xs'
    
    For example, you can change all the letters in a string to 
    lower case by applying the 'toLower' function to the string.
    [Note: toLower must be 'imported' from the Data.Char module
           see the import at the top of this file]
-}
allLowerCase str = [ toLower c | c <- str ]
res3 = allLowerCase madam

{-
    We can combine this concept of 'mapping' with our concept of
    folding and our three earlier specializations of the 'remove' 
    function.

-}
mapRemove str = foldr1 (.) [ remove c | c <- ",. "] str
res4 = mapRemove madam

{-
    So, what did we do here?
    
    [ remove c | c <- ",. "] 'maps' or 'applies' the remove
    function to each character in the provided string so we 
    get a list of the functions:
        [remove ',', remove '.', remove ' ']
        
    And these functions are applied to 'str' and folded up as
    shown in the foldr1 example
    
    If we wanted to remove all punctuation from a string we
    could add other punctuation chars (using a backslash to
    escape single and double quote characters)

-}
removePunctuation str = foldr1 (.) [remove c | c <- ",. \'?\"!;:()"] str
res5 = (allLowerCase . removePunctuation) madam

{-
    We can now write a function that will return True if a string 
    (after all punctuation marks have been removed and all
    characters converted to lower case) is a palindrome.
-}
isPalindrome str = str == (reverse str)
isPalindromic str = 
    (isPalindrome . removePunctuation . allLowerCase) str
                   
res6 = isPalindromic madam
res7 = isPalindromic "A man, a plan, a canal. Panama!"
res8 = isPalindromic "Able was I ere I saw Elba"
res9 = isPalindromic "Able was I ere I saw Chicago"              

{-
    THE CRUCIAL POINT IN ALL THIS IS THAT THE RIGHT-MOST FUNCTION
    MUST RETURN A VALUE OF THE SAME TYPE AS THE ARGUMENT OF THE
    FUNCTION TO IT'S IMMEDIATE LEFT. If this restriction is met,
    the function composition operator can be applied to any two
    functions.
    
    In the formula:  foldr1 f [a,b,c,d]
        - a, b, c and d must have the same type
        - f must deliver a result of the same type as its arguments
        - f must be a function that takes two arguments
-}     
