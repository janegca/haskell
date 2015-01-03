{-
    Problem 20
    
    
    (*) Remove the K'th element from a list.

    Example in Prolog:

    ?- remove_at(X,[a,b,c,d],2,R).
    X = b
    R = [a,c,d]

    Example in Lisp:

    * (remove-at '(a b c d) 2)
    (A C D)

    (Note that this only returns the residue list, while the Prolog 
          version also returns the deleted element.)

    Example in Haskell:

    *Main> removeAt 2 "abcd"
    ('b',"acd")

-}
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (f,s) = splitAt n xs
                in (last f, init f ++ s)
                
-- revised after seeing H99 solutions (head and tail ops less expensive)
removeAt_c n xs = let (f,s) = splitAt (n-1) xs
                  in (head s, f ++ tail s)

-- other methods from H99 site -------------------------------------------
removeAt_a n = (\(a, b) -> (head b, a ++ tail b)) . splitAt (n - 1)

removeAt_b 1 (x:xs) = (x, xs)
removeAt_b n (x:xs) = (l, x:r)
	where (l, r) = removeAt_b (n - 1) xs
                
-- tests -----------------------------------------------------------------
tst f = f 2 "abcd" == ('b', "acd")

tests = tst removeAt && tst removeAt_a && tst removeAt_b
     && tst removeAt_c
