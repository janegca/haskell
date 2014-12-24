-- 1.8 Exercises on High Order Functions

ex6 :: [Int] -> [Bool]
--ex6 xs = [ x == 1 | x <- xs ]
ex6 xs = map (== 1) xs

ex7 :: String -> Bool
ex7 xs = or (map (== '0') xs)