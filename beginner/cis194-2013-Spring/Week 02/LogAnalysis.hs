{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

{-
    Ref:
    http://www.seas.upenn.edu/%7Ecis194/spring13/hw/01-intro.pdf

-}

import Log

{-
    Exercise 1 
    
    The first step is figuring out how to parse an individual
    message. Define a function
    
        parseMessage :: String -> LogMessage
    
    which parses an individual line from the log file. For example,
        parseMessage "E 2 562 help help"
            == LogMessage (Error 2) 562 "help help"
            
        parseMessage "I 29 la la la"
            == LogMessage Info 29 "la la la"
        
        parseMessage "This is not in the right format"
            == Unknown "This is not in the right format"

    Once we can parse one log message, we can parse a whole log file.
    Define a function
    
        parse :: String -> [LogMessage]
    
    which parses an entire log file at once and returns its contents as a
    list of LogMessages.  

    To test your function, use the testParse function provided in the
    Log module, giving it as arguments your parse function, the number
    of lines to parse, and the log file to parse from (which should also be
    in the same folder as your assignment). For example, after loading
    your assignment into GHCi, type something like this at the prompt:
    
        testParse parse 10 "error.log"
    
    Don’t reinvent the wheel! (That’s so last week.) Use Prelude functions
    to make your solution as concise, high-level, and functional as
    possible. For example, to convert a String like "562" into an Int, you
    can use the read function. Other functions which may (or may not)
    be useful to you include lines, words, unwords, take, drop, and (.). 
    
-}

parseMessage :: String -> LogMessage
parseMessage str = pmsg (words str)
    where
        pmsg m@(x:y:z:zs) 
            | x == "I"  = LogMessage Info    (toInt y) (unwords (z:zs))
            | x == "W"  = LogMessage Warning (toInt y) (unwords(z:zs))
            | x == "E"  = LogMessage (Error  (toInt y)) 
                            (toInt z) (unwords zs)
            | otherwise = Unknown (unwords m)
        pmsg m          = Unknown (unwords m)
        
        toInt :: String -> Int
        toInt n = read n :: Int
        
parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)       
    
msg1, msg2, msg3, msg4 :: String    
msg1 = "E 2 562 help help"
msg2 = "I 29 la la la"
msg3 = "This is not in the right format"
msg4 = "W 1654 'I kept all my limbs very supple"

ex1a, ex1b, ex1c, ex1d, ex1test :: Bool
ex1a = parseMessage msg1 == LogMessage (Error 2) 562 "help help"
ex1b = parseMessage msg2 == LogMessage Info 29 "la la la"
ex1c = parseMessage msg3 == Unknown "This is not in the right format"  
ex1d = parseMessage msg4 
            == LogMessage Warning 1654 "'I kept all my limbs very supple"
ex1test = ex1a && ex1b && ex1c && ex1d

ex1testa :: IO [LogMessage]
ex1testa = testParse parse 10 "error.log"

{-

    Exercise 2 
    
    Define a function
    
        insert :: LogMessage -> MessageTree -> MessageTree
    
    which inserts a new LogMessage into an existing MessageTree, producing
    a new MessageTree. insert may assume that it is given a
    sorted MessageTree, and must produce a new sorted MessageTree
    containing the new LogMessage in addition to the contents of the
    original MessageTree.
    
    However, note that if insert is given a LogMessage which is
    Unknown, it should return the MessageTree unchanged.
    
    Note: sort should be on the message timestamp

-}
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf      = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node t1 m@(LogMessage _ mts _) t2) 
    | ts <= mts = Node (insert msg t1) m t2
    | otherwise = Node t1 m (insert msg t2)
insert _ t = t
    
                          

