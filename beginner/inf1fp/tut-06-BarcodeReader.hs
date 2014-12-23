-- Informatics 1 Functional Programming
-- Tutorial 6 Barcode Reader
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/tutorial6.zip

import Test.QuickCheck
import System.Random

-- Importing the keymap module
--import KeymapList         -- uncomment for questions 1 thru 4
import KeymapTree           -- add after completing questions 1 thru 4

-- Type declarations
type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

-- matches Keymap constructor with the Barcode becoming the key
-- and the Item, the key's associated value
type Catalogue = Keymap Barcode Item    


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


{- Exercise 1
   (a) Write a function longestProductLen that finds the length of the 
       longest product name in a list of (Barcode,Item)-pairs.
    
        Example
            *Main> longestProductLen (toList testDB)
             57
-}

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen = maximum . map (length . fst . snd)

{-
    (b) Write a function formatLine that, given a number (the desired 
        length of the product name) prints the barcode, product and unit 
        information, separated by dots, as a single line. For example:
        
            *Main> formatLine 7 ("0001",("product","unit"))
            "0001...product...unit"
            *Main> formatLine 7 ("0002",("thing","unknown"))
            "0002...thing.....unknown"

        You may assume that the product name is never longer than the 
        desired length for it.
-}
formatLine :: Int -> (Barcode, Item) -> String
formatLine n (bc, (prod,unit)) 
    = bc ++ "..." ++ prod ++ replicate (n - (length prod) + 3) '.' ++ unit

{-
    (c) Write a function showCatalogue that pretty-prints a Catalogue. 
        You will need to use toList (from the KeymapList module). Test 
        your function by writing at the prompt:
            *Main> putStr (showCatalogue testDB)
-}    
showCatalogue :: Catalogue -> String
showCatalogue catalogue = unlines (map fmt cat)
    where
        cat = toList catalogue
        fmt = formatLine (longestProductLen cat)
     
{- Exercise 2

    Next, we will start using the get function.
    Firstly, try the following at the interpreter:
        *Main> :t get
        
    You will see that the result type is Maybe a. This data-type is 
    defined as follows:
    
            Maybe a = Nothing
            | Just a
            
    A value of type Maybe a is either the value Nothing, or it is a value 
    of the form Just x. In this way, values of type Maybe a can be used to
    represent ordinary values that have the option of failing. The type is
    just like the type a, but it contains the extra value Nothing meaning 
    "there is no value."
    
    So by saying that get returns a value of type Maybe a, we are saying 
    that it will either return a value of the form Just x if x appears in 
    the database, or otherwise will return Nothing.
    
    Now try the following expressions at the interpreter:
        *Main> get "9780201342758" testDB
        *Main> get "000" testDB
        
    What did you get?
    
        *Main> get "9780201342758" testDB
        Just ("Thompson - \"Haskell: The Craft of Functional Programming\
           "","Book")
        *Main> get "000" testDB
        Nothing
    
    (a) When you apply your function get with a certain key to the test 
        database, what is the type it returns? What are the possible 
        values (hint: there are five)?
        
            Nothing plus the 4 Item values in testDB
    
-}
{-
    (b) Write a function 
            maybeToList :: Maybe a -> [a] 
            
        that returns the empty list if the 
        input is Nothing, and returns a singleton list otherwise.
-}
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

{-
    (c) Write another function 
            listToMaybe :: [a] -> Maybe a. 
        
        You can think of this function as a safe version of head. It 
        should return Nothing if the input is the empty list, and return 
        just the head otherwise.
-}
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

{-
    (c) Write the function 
            catMaybes :: [Maybe a] -> [a]. 
            
        This function should drop all elements of the form Nothing, and 
        strip the Just constructor from all the other elements.
        
        Note: 'concat' ignores 'Nothing' elements
-}
catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

{- Exercise 3
    Using the functions from the previous exercise, Write a function 
    getItems that, given a list of Barcodes, returns a list of Items. 
    Test your code for errors by typing:
        *Main> getItems ["0001","9780201342758","0003"] testDB
        
    It should return a list containing only the item for the textbook.
-}

getItems :: [Barcode] -> Catalogue -> [Item]
getItems codes cat = catMaybes [ get bc cat | bc <- codes ]

-- provided solution
-- 'flip' reverses the order of the 'get' function's arguments
--  so: 'get key db' becomes 'get db key' allowing the function
--  to be mapped to the list of barcodes
getItems' :: [Barcode] -> Catalogue -> [Item]
getItems' xs cat = catMaybes $ map (flip get cat) xs

-- Input-output ------------------------------------------
{-
    Include functions to work with a provided database:
    
        *Main> theDB <- readDB          -- reads database.csv
        Done
        
        *Main> size theDB               -- size of the .csv db
        104651
        
        *Main> getSample theDB          -- random barcode from the db
        "0634479353826"
        
        *Main> get it theDB             -- item assoc with the barcode
        Just ("Colin Kadey: Coming Into Focus","CD")
        
        *Main> :set +s                  -- turns on GHCi timekeeping feature
        
        *Main> getSample theDB          -- now reports fn execution time
        "9300110076157"
        (0.02 secs, 0 bytes)

-}
readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
                  
{- Exercise 4
    (a) Reload your file and load up the database again. Take a note of 
        how much time it took.
        
            *Main> :r
            Ok, modules loaded: KeymapList, Main.
            *Main> theDB <- readDB
            Done
            (1.52 secs, 827921664 bytes)
        
    (b) Take at least ten samples from the database, and record how much 
        time it takes to find an item with get. (The time it takes to find
        a random sample is not really relevant here.)
        
            (7 x 0.02 secs + 2 x 0.03 secs + 1 x 0.00 secs) / 10
            = 0.02 secs on average
            
    (c) Think about the different values you get. If the database was 2 
        times bigger, how much longer would it take (on average) to find 
        an item? How many items does the get function from KeymapList look 
        at before it finds the right item, if it happens to be the last 
        one?
        
            if the DB is doubled, 'get' time will double
            if the item being retrieved is the last item 'get' will
                look at every other item in the DB
        
-}
{-
    Exercises 5 thru 10 - see KeymapTree.hs
-}
{-
    11.
    (a) Load up the database again by entering 
           theDB <- readDB 
           
        at the prompt. This time, it will be constructed as a tree. 
        How much time did it take?
        
            *Main> theDB <- readDB
            Done
            (9.77 secs, 2065496896 bytes)
            
        (original took approx 2 secs)

    (b) Try on at least 10 examples how fast your get function is now. 
        Remember:
                *Main> getSample theDB
        
        gives you a random barcode, and then
            *Main> get it theDB
        
        looks up the associated item.
        
            average time of get is 1 second
        
    (c) How many barcodes does our get function inspect, at most, when 
        searching the database?
        
            the depth of the tree, 40 for theDB
        
        
-}
        
        
