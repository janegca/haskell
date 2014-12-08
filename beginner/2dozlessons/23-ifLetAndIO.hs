-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 23 - If, Let and Unlimited Interactive Input

{-
    do clauses impose a top-down order on directives
    can't use 'where' clauses within a 'do'
    
    let - used to assign values to local variables
        - the values are visible only within the defining do clause
        
    if-then-else clause tests conditions similar to guards
    
    This example code asks a user to enter names and sorts them
    when he quits:
    
        *Main> main
        Enter name (or "no more names" to terminate): Ringo Starr
        Enter name (or "no more names" to terminate): John Lennon
        Enter name (or "no more names" to terminate): George Harrison
        Enter name (or "no more names" to terminate): Paul McCartney
        Enter name (or "no more names" to terminate): no more names
        George Harrison
        John Lennon
        Paul McCartney
        Ringo Starr    
-}
import Data.Char(toLower)
import SequenceUtilities(quicksortWith)

main =
    do
        names <- getNames
        do
            let sortedNames = quicksortWith namePrecedes names
            putStr(unlines sortedNames)

getNames =
    do
        name <- getName
        if name == "no more names"
            then return [ ]
            else
                do
                    names <- getNames
                    return([name] ++ names)

getName =
    do
        putStr "Enter name (or \"no more names\" to terminate): "
        name <- getLine
        return name

namePrecedes name1 name2 = precedesAlphabetically lnf1 lnf2
    where
        lnf1 = lastNameFirst name1
        lnf2 = lastNameFirst name2

lastNameFirst name =
    dropWhile (== ' ') separatorThenLastName ++ " " ++ firstName
    where
        (firstName, separatorThenLastName) = break (== ' ') name

precedesAlphabetically :: String -> String -> Bool
precedesAlphabetically x y
    | xLower == yLower = x < y
    | otherwise = xLower < yLower
    where
        xLower = map toLower x
        yLower = map toLower y
