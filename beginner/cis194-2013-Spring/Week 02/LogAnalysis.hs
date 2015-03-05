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
    
{-
    Exercise 3 

    Once we can insert a single LogMessage into a MessageTree,
    we can build a complete MessageTree from a list of messages. 
    Specifically, define a function
    
        build :: [LogMessage] -> MessageTree
        
    which builds up a MessageTree containing the messages in the list,
    by successively inserting the messages into a MessageTree (beginning
    with a Leaf).

-}                          
build :: [LogMessage] -> MessageTree
build msgs = bld msgs Leaf
    where
        bld [] t     = t
        bld (x:xs) t = bld xs (insert x t)
   
exBuild :: MessageTree   
exBuild =  build (map parseMessage [msg1, msg2,msg3,msg4])

{-
    Exercise 4 

    Finally, define the function

        inOrder :: MessageTree -> [LogMessage]

    which takes a sorted MessageTree and produces a list of all the
    LogMessages it contains, sorted by timestamp from smallest to biggest.
    (This is known as an in-order traversal of the MessageTree.)
    
    With these functions, we can now remove Unknown messages and
    sort the well-formed messages using an expression such as:

        inOrder (build tree)
    
    [Note: there are much better ways to sort a list; this is just an exercise
    to get you working with recursive data structures!]

-}       
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf           = []
inOrder (Node t1 m t2) = inOrder t1 ++ m : inOrder t2

{-
    Exercise 5 

    Now that we can sort the log messages, the only thing
    left to do is extract the relevant information. We have decided that
    “relevant” means “errors with a severity of at least 50”.
    
    Write a function

        whatWentWrong :: [LogMessage] -> [String]

    which takes an unsorted list of LogMessages, and returns a list of the
    messages corresponding to any errors with a severity of 50 or greater,
    sorted by timestamp. (Of course, you can use your functions from the
    previous exercises to do the sorting.)
    
    For example, suppose our log file looked like this:
    
        I 6 Completed armadillo processing
        I 1 Nothing to report
        E 99 10 Flange failed!
        I 4 Everything normal
        I 11 Initiating self-destruct sequence
        E 70 3 Way too many pickles
        E 65 8 Bad pickle-flange interaction detected
        W 5 Flange is due for a check-up
        I 7 Out for lunch, back in two time steps
        E 20 2 Too many pickles
        I 9 Back from lunch
        
        
    This file is provided as sample.log. There are four errors, three of
    which have a severity of greater than 50. The output of whatWentWrong
    on sample.log ought to be

        [ "Way too many pickles"
        , "Bad pickle-flange interaction detected"
        , "Flange failed!"
        ]

    You can test your whatWentWrong function with testWhatWentWrong,
    which is also provided by the Log module. You should provide
    testWhatWentWrong with your parse function, your whatWentWrong
    function, and the name of the log file to parse.    

-}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []     = []
whatWentWrong ((LogMessage (Error n) _ txt) : xs) | n > 50
                     = txt : whatWentWrong xs
whatWentWrong (_:xs) = whatWentWrong xs

ex5test :: IO [String]
ex5test = testWhatWentWrong parse whatWentWrong "sample.log"

ex6 :: IO [String]
ex6 = testWhatWentWrong parse whatWentWrong "error.log"

    