{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-
    CIS 552: Advanced Programming (2015)
    Homework 2 - Datatypes and Trees

    Problem 3 - First worked solution to problem 3
        
    Transform a  SimpleXML document from one form (a PLAY) to
    another (HTML).
    
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
  
    Referenced solution from:
    https://github.com/Gabriel0402/CIS-552/hw2-trees-XML-parsing/Main.hs
  
-}
import Prelude hiding (takeWhile,all)
import Test.HUnit      -- unit test support
import XMLTypes        -- support file for problem 3 (provided)
import Play            -- support file for problem 3 (provided)

formatPlay :: SimpleXML -> SimpleXML
formatPlay (Element "PLAY" children) =
    Element "html" [Element "body" (concatMap formatText children)]
formatPlay _ = error "formatPlay: not a PLAY element"

formatText :: SimpleXML -> [SimpleXML]
formatText (Element "TITLE" t)           = [Element "h1" t]
formatText (Element "PERSONAE" children) = formatPersonae children
formatText (Element "ACT" children)      = concatMap formatActs children
formatText xml = [xml]
    
formatTitle :: HtmlTag -> SimpleXML -> SimpleXML
formatTitle htag (Element "TITLE" s) = Element htag s
formatTitle _ _ = error "formatTitle: not a TITLE element"
        
formatPersonae :: [SimpleXML] -> [SimpleXML]
formatPersonae xml 
    = [Element "h2" [PCDATA "Dramatis Personae"]] ++ persona xml
    where
        persona ((Element "PERSONA" [d]):xs) 
            = d : Element "br" [] : persona xs
        persona [] = []
        persona _ = error "formatPersonae: not a PERSONA element"
    
formatActs :: SimpleXML -> [SimpleXML]
formatActs (Element "TITLE" t) = [Element "h2" t]
formatActs (Element "SCENE" children) = concatMap formatScenes children
formatActs xml = [xml]

formatScenes :: SimpleXML -> [SimpleXML]
formatScenes (Element "TITLE" t)    = [Element "h3" t]
formatScenes (Element "SPEECH" txt) = concatMap formatSpeech txt
formatScenes xml = [xml]

formatSpeech :: SimpleXML -> [SimpleXML]
formatSpeech (Element "SPEAKER" s)  = [Element "b" s, Element "br" []]
formatSpeech (Element "LINE" (s:[])) = [s , Element "br" []]
formatSpeech xml = [xml]

-- playing with given method and structures to get a feel for them
p1 :: String
p1 = xml2string play

smpPlay,smpPlay2, title :: SimpleXML
smpPlay = Element "PLAY" 
            [Element "TITLE" [PCDATA "Play title"],
             Element "PERSONAE" 
                [Element "PERSONA" [PCDATA "Person 1"],
                 Element "PERSONA" [PCDATA "Person 2"]],
             Element "ACT" 
              [Element "TITLE" [PCDATA "Act 1"],
                Element "SCENE" 
               [Element "TITLE" [PCDATA "Scene 1"],
                Element "SPEECH" 
                 [Element "SPEAKER" [PCDATA "Person 1"],
                  Element "LINE" [PCDATA "Line 1"],
                  Element "LINE" [PCDATA "Line 2"]]]]]
                  
smpPlay2 = Element "PLAY" [
            Element "TITLE" [PCDATA "Play2 title"],
            Element "PERSONAE" personae,
            Element "ACT" act,
            Element "ACT" act2]
            
title = Element "TITLE" [PCDATA "Play title"]            
         
personae, act, act2, scene, speaker1,speaker2 :: [SimpleXML]         
personae = [Element "PERSONA" [PCDATA "Person 1"],
            Element "PERSONA" [PCDATA "Person 2"]]
            
act = [Element "TITLE" [PCDATA "Act 1"],
       Element "SCENE" scene]
       
act2 = [Element "TITLE" [PCDATA "Act 2"],
       Element "SCENE" scene]       

scene = [Element "TITLE" [PCDATA "Scene 1"], 
         Element "SPEECH" speaker1, 
         Element "SPEECH" speaker2]
         
speaker1 = [Element "SPEAKER" [PCDATA "Person 1"],
           Element "LINE" [PCDATA "Line 1"],
           Element "LINE" [PCDATA "Line 2"]]
           
speaker2 = [Element "SPEAKER" [PCDATA "Person 2"],
            Element "LINE" [PCDATA "Line 1"],
            Element "LINE" [PCDATA "Line 2"]]
                        
p2 :: String                  
p2 = xml2string smpPlay  
 
-- generic functions to examine the SimpleXML structure  
-- took a stab at these but didn't use them in solution
type HtmlTag = String         
type Dict = [(String, String)]

pdict :: Dict
pdict = [ ("PLAY",   "html"),
          ("TITLE",    "h1"),
          ("PERSONAE", "h2"),
          ("ACT",      "h2"),
          ("SCENE",    "h3"),
          ("SPEAKER",   "b"),
          ("LINE",      "br"),
          ("PERSONA",   "br") ]
          
tagToHtml :: Dict -> String -> String
tagToHtml dict tag = html (lookup tag dict)
    where html Nothing = ""
          html (Just x) = x

tTagToHtml :: IO Counts          
tTagToHtml = runTestTT (TestList 
    ["tth0" ~: tagToHtml pdict "PLAY" ~?= "html",
     "tth1" ~: tagToHtml pdict "ACT" ~?= "h2",
     "tth2" ~: tagToHtml pdict "PERSONA" ~?= "br"])          

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
