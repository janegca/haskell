Discrete Mathematics
03.2 Higher Order Recursive Functions

Most recursive functions follow a computing pattern that can be
generalized into another 'higher order function'. For example,
functions that take a list as an argument, transform each element
in some mannter and then returned the transformed elements in
a new list can be generalized into a 'map' function.

        map :: (a->b) -> [a] -> [b]
        map f [] = []
        map f (x:xs) = f x : map f xs

'map' takes function (a->b), a list, [a], and returns a new list, [b].
The function is applied to each element in the input list.

An example walkthrough:

    map (*5) [1,2,3]
    = 1*5 : map (*5) [2,3]
    = 1*5 : (2*5 : map (*5) [3])
    = 1*5 : (2*5 : (3*5 : map (*5) []))
    = 1*5 : (2*5 : (3*5 : []))
    = 5 : (10 : (15 : []))
    = [5,10,15]       
    
If we have a function that takes two arguments and we want to apply
it to the corresponding elements of two lists, we can use 'zipWith'

        zipWith :: (a->b->c) -> [a] -> [b] -> [c]
        zipWith f [] ys = []
        zipWith f xs [] = []
        zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys    

'zipWith' takes a function that takes 2 args, (a->b->c), and two lists,
[a], [b]; returning a new list, [c]. The function is applied to the
corresponding elements from the two lists.

Sometimes we want to reduce a list to a single value, for example, when
we take the length or sum of a list. This pattern is captured by 'foldr'

        foldr :: (a->b->b) -> b -> [a] -> b
        foldr f z [] = z
        foldr f z (x:xs) = f x (foldr f z xs)    

'foldr' takes a binary function (a function taking two arguments), 
(a->b->c); a 'starting' or 'zero' value, b, for the reduction and an input
list, [a], and then it returns a single value, b.  
What is different here is that the function arguments are a list element 
AND the REST of the list:
            f x (foldr f z xs)
            
The 'recursive' call is made in the second argument to the function.

Example walkthrough of summing a list:

    foldr (+) 0 [1,2,3]
    = 1 + foldr (+) 0 [2,3]
    = 1 + (2 + foldr (+) 0 [3])
    = 1 + (2 + (3 + foldr (+) 0 []))
    = 1 + (2 + (3 + 0))
    = 6
            
The recursion produces a sequence of results starting with the right end
of the list (hence 'foldr'). The 'z' argument acts as an accumulator 
(usually the identity value for the operation i.e. for additon, 0, for
multiplication, 1, for lists, the empty list, []).

The following functions can all be defined by 'foldr'

        sum xs = foldr (+) 0 xs
        product xs = foldr (*) 1 xs
        and xs = foldr (&&) True xs
        or xs = foldr (||) False xs
        factorial n = foldr (*) 1 [1..n]    

When there is a choice between using a recursive function or a higher
order function to implement a process, it is usually considered good
form to use the higher order function as (a) it is shorter, (b) it
is immeadiately evident what computing pattern is being used.

Example, consider a function that is to return the first element of
a list of pairs. Initially, a the recursive version,

> firsts :: [(a,b)] -> [a]
> firsts [] = []
> firsts ((a,b):ps) = a : firsts ps   

and then using a higher order functin, 'map'

> firsts' :: [(a,b)] -> [a]
> firsts' xs = map fst xs     

> lst = [(1,2),(3,4)]

both produce the same result but the second is shorter and the 'map'
notifies us that each element in the list is being reduced to the
first element in the pair.

        Main> lst
        [(1,2),(3,4)]
        Main> firsts lst
        [1,3]
        Main> firsts' lst
        [1,3]

        Main> firsts lst == firsts' lst
        True

        
        
