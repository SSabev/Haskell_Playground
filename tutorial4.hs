-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (29/30 Oct)


import System
import IO
import List( nub )
import Char
import Test.QuickCheck

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

instance Arbitrary Char where
   arbitrary     = choose ('\32', '\128')
   coarbitrary c = variant (ord c `rem` 4) 

tutorialURL = "http://www.inf.ed.ac.uk/admin/itodb/mgroups/tuts/inf1-fp.html"
groupURL    = "http://www.inf.ed.ac.uk/admin/itodb/mgroups/stus/inf1-fp.html"
testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:w.b.heijltjes@sms.ed.ac.uk\">Willem Heijltjes</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br><b>TA:</b> "
            , "mailto:w.b.heijltjes@sms.ed.ac.uk\">Willem Heijltjes</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Willem Heijltjes","w.b.heijltjes@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url =
    do username <- getEnv "LOGNAME"
       system ("wget -O /tmp/fp-tmp-" ++ username ++ ".html --quiet " ++ url)
       readFile ("/tmp/fp-tmp-" ++ username ++ ".html")

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString [] [] = True
sameString (x:xs) [] = False
sameString [] (y:ys) = False
sameString (x:xs) (y:ys) | toLower x == toLower y = sameString xs ys
                         | otherwise              = False

prop_sameString :: String -> Bool
prop_sameString str  = 
    map toLower str `sameString` map toUpper str

-- 2.
prefix :: String -> String -> Bool
prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) | toLower x == toLower y = prefix xs ys
                     | otherwise              = False

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.

helperF :: String -> String
helperF [] = []   
helperF (x:xs) = toLower x : helperF xs

contains :: String -> String -> Bool
contains [] ys = False
contains xs [] = False
contains xs ys | helperF (xs) /= helperF(take (length xs) ys) = contains xs (drop 1 ys)
               | otherwise                                    = True

prop_contains :: String -> Int -> Int -> Bool
prop_contains = undefined


-- 4.
takeUntil :: String -> String -> String
takeUntil [] ys = []
takeUntil xs [] = []
takeUntil xs ys | helperF xs /= helperF(take (length xs) ys) =  take 1 ys ++ takeUntil xs (drop 1 ys)
                | otherwise = []
                

dropUntil :: String -> String -> String
dropUntil xs ys = drop (length (takeUntil xs ys) + length xs) ys


-- 5.
split :: String -> String -> [String]
split [] ys = error "moron"
split xs [] = [] 
split xs ys  = takeUntil xs ys: split xs (dropUntil xs ys)

reconstruct :: String -> [String] -> String
reconstruct xs [] = []
reconstruct xs (y:ys) = y ++ concat (map (xs++) ys)


prop_split :: String -> String -> Property
prop_split sep str  =  sep /= [] ==> reconstruct sep (split sep str) `sameString` str


-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML [] = []
linksFromHTML xs | take 8 xs == "<a href=" = takeUntil ">" (drop 9 xs) : linksFromHTML (drop (length (takeUntil ">" (drop 9 xs)) + 8 ) xs)
                 | otherwise = linksFromHTML (drop 1 xs) 

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails [] = []
takeEmails (x:xs) | take 6 x == "mailto" = x : takeEmails xs
                  | otherwise            = takeEmails xs


-- 8.

link2pair :: Link -> (Name, Link)
link2pair xs = head (zip (takeUntil "<" (dropUntil ">" xs) :[]) (take (length(takeUntil ">" (drop 7 xs)) -1) (takeUntil ">" (drop 7 xs)) : []))




-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML [] = []
emailsFromHTML xs = nub(link2pair (head (takeEmails (linksFromHTML xs))) : emailsFromHTML (tail xs))

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail = undefined


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML = undefined


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
