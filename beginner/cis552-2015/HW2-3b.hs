{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-
    CIS 552: Advanced Programming (2015)
    Homework 2 - Datatypes and Trees

    Problem 3 - First Refactor of problem 3 solution (see HW2-3a.hs)
-}
{- 3
    Transform a  SimpleXML document from one form (a PLAY) to
    another (HTML).
    
    The purpose of this assignment is not just to “get the job done”—i.e., to produce the right HTML. A more important goal is to think about what is a good way to do this job, and jobs like it.

    To this end, your solution should be organized into two parts:

     1. a collection of generic functions for transforming XML 
        structures  that have nothing to do with plays, plus

     2. a short piece of code (a single function definition or a 
        collection of short functions) that uses the generic functions 
        to do the particular job of transforming a play into HTML.
       
    Obviously, there are many ways to do the first part. The main challenge of the assignment is to find a clean design that matches the needs of the second part. You will be graded not only on correctness (producing the required output), but also on the elegance of your solution and the clarity and readability of your code and documentation. As always, style most definitely counts.    
    
    Notes:
    
    Basic mapping of Play to HTML:
    
    0   PLAY                                    html, body
    1   |- TITLE    [PCDATA]                    h1
    1   |- PERSONAE                             h2, "Dramatis Personae"
    2       |- PERSONA* [PCDATA]                br
    1   |- ACT*                                 
    2       |- TITLE    [PCDATA]                h2
    2       |- SCENE*
    3            |- TITLE   [PCDATA]            h3
    3            |- SPEECH*
    4                 |- SPEAKER  [PCDATA]      b, br
    4                 |- LINE*    [PCDATA]      br
                      
    (*) 1 or more           

    HTML as SimpleXML structure:
        html
          |- body
              |- h1  [PCDATA]
              |- h2* [PCDATA]
                  |- h3* [PCDATA]
                      |- b* [PCDATA]    -- b and br not really elements
                      |- br* [PCDATA]   -- every PCDATA followed by a br
                      
    Refactoring Notes:
    =================
    
    1. Removed code not actually used in original solution.
    
    2. Modified handling of PERSONAE and PERSONA to match pattern
       used by other elements with children
              
    3. Reduced multiple format routines to 'formatChildren'
       Problem now is with getting the right header level for
       TITLE elements
       
    4. Realized you can pattern match on the 'children' list,
       modified ACT and SCENE formatting to look for TITLE as
       first child element.
       
    5. Modified elements that required a new suffix element 
       (PERSONA, SPEAKER, LINE) to follow a consistent pattern
       
    Next step: 
        identify transform patterns and attempt to capture them
        in generic functions.
       
-}
import Prelude hiding (takeWhile,all)
import Test.HUnit      -- unit test support
import XMLTypes        -- support file for problem 3 (provided)
import Play            -- support file for problem 3 (provided)

-- second solution, a refactoring HW2-3a.hs code
formatPlay :: SimpleXML -> SimpleXML
formatPlay (Element "PLAY" children) =
    Element "html" [Element "body" (concatMap formatChildren children)]
formatPlay _ =  error "formatPlay: not a PLAY element"    
    
formatChildren :: SimpleXML -> [SimpleXML]
formatChildren (Element "TITLE" t)           = [Element "h1" t]
formatChildren (Element "PERSONAE" children) =
    [Element "h2" [PCDATA "Dramatis Personae"]] ++
    concatMap formatChildren children
formatChildren (Element "PERSONA" s) = s ++ [Element "br" []]   
formatChildren (Element "ACT" ((Element "TITLE" t): xs)) = 
    [Element "h2" t] ++ concatMap formatChildren xs
formatChildren (Element "SCENE" ((Element "TITLE" t): xs)) = 
    [Element "h3" t] ++ concatMap formatChildren xs  
formatChildren (Element "SPEECH" children)  = 
    concatMap formatChildren children
formatChildren (Element "SPEAKER" s)  = 
    [Element "b" s] ++ [Element "br" []]
formatChildren (Element "LINE" s)     = s ++ [Element "br" []]
formatChildren xml = [xml]

-- provided test code
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ take 20 cs ++ 
            "' vs '" ++ take 20 ds

test3 :: Test
test3 = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

runTest3 = runTestTT test3