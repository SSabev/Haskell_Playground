import Data.List
import Data.Char
import Test.QuickCheck

instance Arbitrary Char where
   arbitrary     = choose ('\32', '\128')
   coarbitrary c = variant (ord c `rem` 4) 

f :: Char -> Char
f x | (ord x) >= 65 && (ord x) <=90 = chr (155 - ord x)
    | otherwise = error "ERROR"

g :: String -> String
g xs = [f x| x<-xs, (ord x) >= 65 && (ord x) <=90]

h :: String -> String
h [] = []
h (x:xs) | (ord x) >= 65 && (ord x) <=90 = chr(155 - ord x): h xs
         | otherwise                     = h xs

c :: String -> String
c [] = []
c xs  =  [ x | (x,i) <- zip xs [0..], even i ]


d :: String -> String
d [] = []
d [x] = [x]
d (x:xs) = x : d (tail xs)

prop_cd :: String -> Bool
prop_cd x = c x == d x
