import Data.Char
import Data.List
import Test.QuickCheck

instance Arbitrary Char where
   arbitrary     = choose ('\32', '\128')
   coarbitrary c = variant (ord c `rem` 4)

f :: Char -> Int
f x | isAlpha x && elem x ['a'..'m'] = 1
    | isAlpha x && elem x ['A'..'M'] = 3
    | isAlpha x && elem x ['n'..'z'] = 2
    | isAlpha x && elem x ['N'..'Z'] = 6
    | otherwise = error "Not valid"

g :: String -> Int
g xs = sum [f x | x<-xs, isAlpha x]

h :: String -> Int
h [] = 0
h (x:xs) | isAlpha x = f x + h xs
         | otherwise = h xs

c :: [Int] -> Bool
c xs = ((length xs) -1) == length ([True| x<-xs, x> head (tail xs)])

d :: [Int] -> Bool
d [x] = True
d (x:xs) | x > head xs = d xs
         | otherwise = False
