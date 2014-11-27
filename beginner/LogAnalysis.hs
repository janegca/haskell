{-
    Exercise: Log file parsing
    Source:
        CIS 194: Introduction to Haskell (Fall 2014), Richard Eisenberg
        Week 3 Homework http://www.seas.upenn.edu/~cis194/lectures.html
        
        The files Log.hs, error.log, sample.log were d/l from:
        http://www.seas.upenn.edu/~cis194/extras/03-ADTs/Log.hs
        http://www.seas.upenn.edu/~cis194/extras/03-ADTs/error.log
        http://www.seas.upenn.edu/~cis194/extras/03-ADTs/sample.log
        
    Notes:                      
        1. Make sure you understand type creation for MessageType
           LogMessage and MaybeLogMessage by playing with them
           in ghci
           
        2. 'parseMessage' was moderately difficult; debated between
           a single function or multiple smaller functions; in the
           end, creating separate functions for ValidLM and InvalidLM 
           didn't make the code any clearer so stuck to the one function. 
           (see note on refactoring at end of this file)

-}

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.List ( sortBy, isInfixOf )
import Data.Char ( toLower )

{-
    An 'error.log' file
    ---------------------------
        The file consists of lines of text
        Each line begins with a single character indicating the msg type
            'I'     informational
            'W'     warning
            'E'     error
        Error msg lines are followed by a number indicating the severity
            of the error with 1 being the lowest
        All msg types have an integer timestamp followed by the message
        
        Ex
           I 147 mice in the air, I’m afraid, but you might catch a bat
           E 2 148 #56k istereadeat lo d200ff] BOOTMEM
           
    The basic types are defined in 'Log.hs'
        data MessageType = Info
                         | Warning
                         | Error Int
            deriving (Show, Eq)

        type TimeStamp = Int

        data LogMessage = LogMessage MessageType TimeStamp String
            deriving (Show, Eq)
            
        data MaybeLogMessage = ValidLM LogMessage
                             | InvalidLM String
            deriving (Show, Eq)
        
    General Instructions for building solutions
    -------------------------------------------
        Use Prelude functions to make your solution as concise, 
        high-level, and functional as possible. Functions which may 
        (or may not) be useful to you include 
             lines, words, unwords, take, drop, (.), map, and filter
-}

-- found it useful to mimic a long string coming from a file for testing 
-- things directly within ghci before using the supplied test functions
-- i.e. parse testMsgs
testMsgs :: String      
testMsgs =  "I 6 Completed armadillo processing \n"
         ++ "E 99 10 Flange failed! \n"
         ++ "E 70 3 Way too many pickles \n"
         ++ "E 65 8 Bad pickle-flange interaction detected \n"
         ++ "W 5 Flange is due for a check-up \n"
         ++ "I 100 Relishsign detected!"

{-
    Ex 1
    Define a function which parses an individual line from the log file.
    There is a very useful function toward the bottom of Log.hs that
    you will need to use here! (readInt)
    
    Examples:
        parseMessage "E 2 562 help help"
            == ValidLM (LogMessage (Error 2) 562 "help help")
        
        parseMessage "I 29 la la la"
            == ValidLM (LogMessage Info 29 "la la la")
        
        parseMessage "This is not in the right format"
            == InvalidLM "This is not in the right format"    
-}
parseMessage :: String -> MaybeLogMessage
parseMessage message = buildMsg (words message)
    where
        buildMsg :: [String] -> MaybeLogMessage
        buildMsg msg@(x : y : z : xs)
            | readInt y == InvalidInt = InvalidLM (unwords msg)
            | x == "I"                = ValidLM
                                         (LogMessage Info (strToInt y) 
                                         (z ++ (unwords xs)))
            | x == "W"                = ValidLM
                                         (LogMessage Warning (strToInt y) 
                                         (z ++ unwords(xs)))
            | x == "E" && 
              readInt z /= InvalidInt = ValidLM (LogMessage
                                          (Error (strToInt y)) (strToInt z) 
                                          (unwords xs))
            | otherwise               = InvalidLM (unwords msg)
        buildMsg msg = InvalidLM (unwords msg)

        -- convert an integer string to an Int        
        strToInt :: String -> Int
        strToInt str = read str :: Int

{-
    Ex 2
    Write a function that throws out invalid messages
-}        
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly []                   = []
validMessagesOnly ((ValidLM x) : xs)   = x : validMessagesOnly xs
validMessagesOnly (_ :xs)              = validMessagesOnly xs

{-
    Ex 3
    
    Now, we can put these pieces together to define
        parse :: String -> [LogMessage]
    which parses an entire log file at once and returns its contents as a
    list of LogMessages.
    
    To test your function, use the testParse function provided in the
    Log module, giving it as arguments your parse function, the number
    of lines to parse, and the log file to parse from (which should also be
    in the same folder as your assignment). For example, after loading
    your assignment into GHCi, type something like this at the prompt:
        testParse parse 10 "error.log"
-}
parse :: String -> [LogMessage]
parse file = validMessagesOnly (map parseMessage (lines file))

{-
    Ex 4
    
    Define a function that compares two LogMessage's based on their
    time stamp.
    
    Use the Ordering data type supplied in Prelude:
        data Ordering = LT | EQ | GT
    
    Examples:
    compareMsgs (LogMessage Warning 153 
      "Not a speck of light is showing, so the danger must be growing...")
                (LogMessage Info 208 
                "the Weighted Companion Cube cannot talk") == LT

    compareMsgs (LogMessage (Error 101) 2001 "My God! It’s full of stars!")
                (LogMessage Info 2001 
                "Daisy, Daisy, give me your answer do.") == EQ
-}
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ t1 _) (LogMessage _ t2 _) 
    | t1 < t2   = LT
    | t1 == t2  = EQ
    | otherwise = GT
    
{-

    Ex 5
    
    Now that you have said how to compare messages, you
    can sort the list. Write a function
        sortMessages :: [LogMessage] -> [LogMessage]
    that sorts the list of messages. Do not write out a full sorting algorithm!
    Instead, poke around in the Data.List module looking for a
    very convenient function!    

-}    
sort :: [LogMessage] -> [LogMessage]
sort msgs = sortBy compareMsgs msgs

{-
    Ex 6
    
    Write a function
        whatWentWrong :: [LogMessage] -> [String]
    which takes an unsorted list of LogMessages, and returns a list of the
    messages corresponding to any errors with a severity of 50 or greater,
    sorted by timestamp.
    
    You can test your whatWentWrong function with testWhatWentWrong,
    which is also provided by the Log module. You should provide
    testWhatWentWrong with your parse function, your whatWentWrong
    function, and the name of the log file to parse
    
    Note: originally had x >= 50 as option for severity, this was
          refactored into a separate fn in Ex 8
-}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []    = []
whatWentWrong msgs = filterMsgs (sort msgs)
    where
        filterMsgs :: [LogMessage] -> [String]
        filterMsgs [] = []
        filterMsgs ( msg@(LogMessage (Error _) _ m ) : xs)
            | isSevereError msg = m : filterMsgs xs 
            | otherwise         = filterMsgs xs
        filterMsgs (_:xs)       = filterMsgs xs        
            
{-
    Ex 7
    
    Define a function
        messagesAbout :: String -> [LogMessage] -> [LogMessage]
    that filters a list of LogMessages to include only those messages that
    contain the string provided. Make your function case-insensitive, 
    so that "relish" matches "Relishsign detected!!".
    
    Note: originally had test for 'txt' inclusion in message as part
          of this function; code was refactored in Ex 8
-}
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout _  [] = []
messagesAbout txt ( x : xs)
    | isMsgAbout txt x = x : messagesAbout txt xs
    | otherwise        = messagesAbout txt xs
          
{-
    Ex 8
    
    Write a function that makes a list including both all high-severity 
    errors and all messages containing the provided string. Note that you
    will likely have to edit (that is, refactor) previously-written code 
    to be able to do this without repeating yourself much. To test your 
    function, run something like this in GHCi:
    
      testWhatWentWrong parse (whatWentWrongEnhanced "relish") "error.log"
      
    Kudo's if you can use the (|||) function

-}       
(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x    -- (||) is Haskell’s ordinary "or" operator

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced _ [] = []
whatWentWrongEnhanced srch msgs = enhancedMsgs srch (sort msgs)
    where
        enhancedMsgs :: String -> [LogMessage] -> [String]
        enhancedMsgs _ [] = []
        enhancedMsgs txt ( lm@(LogMessage _ _ m) : ms)
           | (|||) isSevereError 
                   (isMsgAbout txt) lm = m : enhancedMsgs txt ms
           | otherwise                 = enhancedMsgs txt ms
   
-- methods extracted (refactored) from whatWentWrong and messagesAbout 
-- for use in whatWentWrongEnhanced 
isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity >= 50
isSevereError _ = False

isMsgAbout :: String -> LogMessage -> Bool
isMsgAbout txt (LogMessage _ _ m) 
    = map toLower txt `isInfixOf` map toLower m
 
   
{-
    Note on refactoring 'parse':
    
    Originally wrote one rather ungainly looking function 
    for 'parseMessage' which I decided to break up into
    the the 3 functions shown below. In the end, decided they
    didn't make the code any more readable BUT the process of
    creating them did help me  figure out how to tighten up
    my original function.
    
    Leaving them here as a reminder that re-factoring is useful
    even when you don't always use the full refactoring.

checkMsg :: [String] -> MaybeLogMessage
checkMsg msg@(x : y : z : xs)
    | readInt y == InvalidInt = invalidMsg msg
    | x == "E" && 
      readInt z == InvalidInt = invalidMsg msg
    | otherwise               = validMsg x (read y :: Int) z (unwords xs)
checkMsg msg                  = invalidMsg msg
    
invalidMsg :: [String] -> MaybeLogMessage
invalidMsg msg = InvalidLM (unwords msg)

validMsg :: String -> Int -> String -> String -> MaybeLogMessage
validMsg x y z txt
    | x == "I"   = ValidLM (LogMessage Info y (z ++ txt))
    | x == "W"   = ValidLM (LogMessage Warning y (z ++ txt))
    | otherwise  = ValidLM (LogMessage (Error y) (read z :: Int) txt)
-}                                       
                                       
