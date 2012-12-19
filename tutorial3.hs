-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 18/22 Oct.

import Data.Char
import Data.List
import Test.QuickCheck

instance Arbitrary Char where
   arbitrary     = choose ('\32', '\128')
   coarbitrary c = variant (ord c `rem` 4) 


-- 1. Map
-- a.
uppers :: String -> String
uppers xs = map toUpper xs

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (2*) xs

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map penceToP xs where penceToP x = fromIntegral x/100

-- d.
uppers' :: String -> String
uppers' xs = [toUpper x | x<-xs]

prop_uppers :: String -> Bool
prop_uppers xs = uppers' xs == uppers xs



-- 2. Filter a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
rmChar ::  Char -> String -> String
rmChar x xs = filter (/= x) xs

-- c.
above :: Int -> [Int] -> [Int]
above x xs = filter (>=x) xs

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter.uncurry$(/=)

-- e.
rmCharComp :: Char -> String -> String
rmCharComp a xs = [x | x<-xs, x/=a]

prop_rmChar :: Char -> String -> Bool
prop_rmChar a xs = rmChar a xs == rmCharComp a xs


-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s  =  [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' =  filter isAlpha.map toUpper

prop_upperChars :: String -> Bool
prop_upperChars s  =  upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs  =  [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map(2*).filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs  =  largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs  =  [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' xs = map reverse (filter stringy xs) 
                  where stringy xs = even (length xs) == True

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs  =  reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec [] = 1
productRec xs = head xs * productRec(tail xs)

productFold :: [Int] -> Int
productFold  = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs  =  productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) | x == True = andRec xs
              | otherwise = False 

andFold :: [Bool] -> Bool
andFold  xs = length xs == length(foldr (:) [] (filter (/=False) xs))

prop_and :: [Bool] -> Bool
prop_and xs  =  andRec xs == andFold xs 

-- c.
concatRec :: [String] -> String
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [String] -> String
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs  =  concatRec strs == concatFold strs

-- d. (optional)

belongsToStr :: Char -> String -> Bool
belongsToStr n [] = False
belongsToStr n (x:xs) | n /= x    = belongsToStr n xs
                      | otherwise = True 

rmCharsRec :: String -> String -> String
rmCharsRec x [] = []
rmCharsRec x (y:ys) | belongsToStr y x  = rmCharsRec x ys
                    | otherwise         = y : rmCharsRec x ys

belongsToStrAgain :: String -> String -> String
belongsToStrAgain (x:xs) [] = []
belongsToStrAgain (x:xs) (y:ys) | belongsToStr y (x:xs) = belongsToStrAgain (x:xs) ys
                                | otherwise             = y : belongsToStrAgain (x:xs) ys

rmCharsFold :: String -> String -> String
rmCharsFold  = (flip.foldr) rmChar  

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str  =  rmCharsRec chars str == rmCharsFold chars str

-- Optional material

type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform (x:xs) = all (==x) xs

-- b.

thingie :: [[Int]] -> [Int]
thingie [] = []
thingie (x:xs) = length x : thingie xs

valid :: Matrix -> Bool
valid [] = False
valid xs = uniform . thingie$xs


-- 6.
-- b.
size :: Matrix -> (Int,Int)
size xs = (length xs, length (head xs)) 

-- c.
square :: Matrix -> Bool
square xs = length xs == length (head xs)



-- 8.

compareMatrices :: Matrix -> Matrix -> Bool
compareMatrices xs ys | valid xs && valid ys = size xs == size ys
                      | otherwise            = error "SHIT"

plusInts :: [Int] -> [Int] -> [Int]
plusInts [] [] = []  
plusInts (x:xs) (y:ys) = (x + y) : plusInts xs ys 

plusMatrices :: Matrix -> Matrix -> Matrix
plusMatrices [] [] = []
plusMatrices (x:xs) (y:ys) = plusInts x y : plusMatrices xs ys

plusM :: Matrix -> Matrix -> Matrix
plusM xs ys | compareMatrices xs ys = plusMatrices xs ys
            | otherwise             = []

-- 9.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined