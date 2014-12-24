-- simple pattern matching exercises

ex3 :: Char -> Bool
ex3 'a' = True
ex3  c  = False

ex4 :: String -> Bool
ex4 ('h':'e':'l':'l':'o':xs) = True
ex4 xs = False

ex5 :: String -> String
ex5 (' ' : xs) = xs
ex5 xs = xs