{-
    Problem 6
    
    (*) Find out whether a list is a palindrome. A palindrome can be 
        read forward or backward; e.g. (x a m a x).

    Example in Haskell:

    *Main> isPalindrome [1,2,3]
    False
    *Main> isPalindrome "madamimadam"
    True
    *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
    True

-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- other methods from H99 site which don't use reverse -------------------
isPalindrome_a []  = True
isPalindrome_a [_] = True
isPalindrome_a xs  = (head xs) == (last xs) 
                  && (isPalindrome_a $ init $ tail xs)

isPalindrome_b :: (Eq a) => [a] -> Bool     -- 1/2 the number of compares
isPalindrome_b xs = p [] xs xs
   where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
         p rev (x:xs) [_]      = rev == xs
         p rev  xs    []       = rev == xs

-- tests -----------------------------------------------------------------
flst = [1,2,3]
tlst = [1,2,4,8,16,8,4,2,1]

testIntLst  f = f flst == False && f tlst == True
testCharLst f = (f "madamimadam") == True

tests = testIntLst isPalindrome   && testCharLst isPalindrome
     && testIntLst isPalindrome_a && testCharLst isPalindrome_a
     && testIntLst isPalindrome_b && testCharLst isPalindrome_b
