-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (7/8 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ div x 2 | x <- xs, mod x 2  == 0 ]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | mod x 2 == 0    = div x 2 : halveEvensRec xs
                     | otherwise       = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x<-xs, x>=lo, x<=hi ] 

 -- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]  
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | x >= lo && x <= hi  = x : inRangeRec lo hi xs
                        | otherwise           = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs


-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length([x | x<-xs, x>0])

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0     = 1 + countPositivesRec xs
                         | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount n = round(fromIntegral n - 0.1*fromIntegral n)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum([discount(x) | x<-xs, discount(x)<=19900])

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) | discount(x)<=19900 = discount(x) + pennypincherRec xs
                       | otherwise          = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x<-xs, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
                     | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs



-- 6. capitalized

-- List-comprehension version
capitalized :: String -> String
capitalized xs = (toUpper(head xs)) : [toLower x | x<- tail xs]

-- Recursive version
capitalRec :: String -> String
capitalRec [] = []
capitalRec (x:xs) = toLower x: capitalRec xs 

capitalizedRec :: String -> String
capitalizedRec [] = []
capitalizedRec (x:xs) = (toUpper x): capitalRec xs

-- Mutual test
prop_capitalized :: String -> Bool
prop_capitalized = undefined


-- 7. title

-- List-comprehension version
titleCap :: [String] -> [String]
titleCap xs = capitalizedRec(head xs): tail xs

wordCheck :: String -> String
wordCheck xs | length xs >= 4 = capitalized xs
             | otherwise = capitalRec xs

title :: [String] -> [String]
title xs = titleCap([wordCheck x | x<-xs])
                                  
-- Recursive version

titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) | length x>=4 = capitalized x: titleRec xs
                | otherwise   = capitalRec x: titleRec xs

titleCapRec :: [String] -> [String]
titleCapRec xs = titleCap(titleRec xs)

-- mutual test
prop_title :: [String] -> Bool
prop_title = undefined




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter inPosition len words = [ x | x<-words, length x == len, 
-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec = undefined

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind = undefined 



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search = undefined

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec = undefined

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search = undefined


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec = undefined

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined

