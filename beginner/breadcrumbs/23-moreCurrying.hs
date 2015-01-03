-- all of these are equivalent!
f a b = a + b
f' a  = \b -> a + b
f''   = \a b -> a + b
f'''  = \a -> \b -> a + b

{-

    [1 of 1] Compiling Main             ( 23-moreCurrying.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> :t f
    f :: Num a => a -> a -> a           -- constrained to Num type
    *Main> :t f'
    f' :: Num a => a -> a -> a    
    *Main> :t f'''
    f''' :: Integer -> Integer -> Integer  -- must be Integer type
    *Main> :t f''
    f'' :: Integer -> Integer -> Integer
    *Main>
    
    *Main> f 1 2
    3
    *Main> f' 1 2
    3
    *Main> f'' 1 2
    3
    *Main> f''' 1 2
    3
    *Main>     
-}