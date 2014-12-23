-- Informatics 1 Functional Programming
-- Tutorial 6 Barcode Reader
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/tutorial6.zip

-- Indexed data represented as a list


module KeymapList ( Keymap,
                    size,
                    get, set, del,
                    select,
                    toList, fromList
                  )

where

-- newtype's are restricted to one constructor and one field
-- k - the type class of the 'key'
-- a - the type class of the key's associated value
-- K[(k,a)] - a list of stored (key,value) pairs
newtype Keymap k a = K [(k,a)]

-- Functions
--      wherever Eq k appears, the tye type of the key must support
--      equality testing

-- returns the number of entries in a KeyMap
size :: Eq k => Keymap k a -> Int
size (K xs) = length xs

-- return the value of the given key or nothing
get :: Eq k => k -> Keymap k a -> Maybe a
get key (K xs) = lookup key xs

-- given a key and value, sets the value in the KeyMap or
-- adds the new key/value pair
set :: Eq k => k -> a -> Keymap k a -> Keymap k a
set key value (K xs) = K (ins  xs)
    where
      ins [] = [(key,value)]
      ins ((k,v):xs) | k == key  = (k,value) : xs
                     | otherwise = (k,v) : ins xs

-- deletes the given key from the keymap                     
del :: Eq k => k -> Keymap k a -> Keymap k a
del key (K xs) = K (filter ((/=key).fst) xs)

-- select values based on predicate
select :: Eq k => (a -> Bool) -> Keymap k a -> Keymap k a
select f (K xs) = K (filter (f.snd) xs)

-- exports a KeyMap as a list
toList :: Eq k => Keymap k a -> [(k,a)]
toList (K xs) = xs

-- creates a KeyMap from a list of key/value pairs
fromList :: Eq k => [(k,a)] -> Keymap k a
fromList xs = K xs

