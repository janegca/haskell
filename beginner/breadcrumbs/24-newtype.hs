-- try to understand this, it's a pretty common pattern when using 
-- and defining some monads

newtype State s a = State { runState :: (s -> (a,s)) } 

{-
    Notes:
        Source:          
            http://book.realworldhaskell.org/read/using-typeclasses.html
        
        newtype is similar to 'data' but MUST have ONLY ONE constructor
        and that constructor MUST have ONLY ONE FIELD
        
        the purpose of a newtype is to rename an existing type and give
        it a distinct identity
        
        'type' gives another type an alias
        
        'newtype' HIDES another type 
            ie. newtype UniqueId = UniqueId Int
            
                a user WILL NOT KNOW that a UniqueId is implemented
                as an Int [form of encapsulation] nor will it ACT
                as an Int unless we specifically implement it as such
-}

type Id = Int
newtype UniqueId = UniqueId Int

id1 = 10
id2 = 20

unid1 = UniqueId id1
unid2 = UniqueId id2

{-

    *Main> id1 + id2            -- can treat as Int's
    30
    *Main> unid1 + unid2        -- cannot treat as Int's unless you
                                -- provide an instance for (+)

    <interactive>:87:7:
        No instance for (Num UniqueId) arising from a use of ‘+’
        In the expression: unid1 + unid2
        In an equation for ‘it’: it = unid1 + unid2
    *Main> 

-}

{-
    Notes:
        Source: https://www.haskell.org/haskellwiki/Newtype
        
        the following are equivalent:
        
        State    :: (s -> (s, a)) -> State s a
        runState :: State s a     -> (s -> (s, a))

    TODO: need to come back to this after reading more on Monads

-}
