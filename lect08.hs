import Data.List
import Data.Char

ords :: [Char] -> [Int]
ords xs = map ord xs

squares :: [Int] -> [Int]
squares xs = map square xs where square x = x*x

positives :: [Int] -> [Int]
positives xs = filter positive xs where positive x = x > 0

f :: [Int] -> Int
f = foldr (+) 0 . map (^ 2) . filter (> 0)