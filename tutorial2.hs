-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 11/15 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n xs = if  n > 0 && n < length xs then drop n xs ++ take n xs
             else  error "The number you entered is either negative or too big!!!"

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 

listOfPairs = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey n = zip listOfPairs (rotate n listOfPairs)

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp a [] = a
lookUp a (x:xs) | fst x == a = snd x
                | otherwise  = lookUp a xs

-- 5.
encipher :: Int -> Char -> Char
encipher n a | n + ord a <= 90 = chr(n + ord a)
             | otherwise       = chr(n + ord a - 26)

-- 6.
normalize :: String -> String
normalize [] = []
normalize xs | isAlpha(head xs) == True = toUpper(head xs): normalize(tail xs)
             | isDigit(head xs) == True = head xs: normalize(tail xs)
             | otherwise                = normalize(tail xs)

-- 7.

encipherStrRec :: Int -> String -> String
encipherStrRec n [] = []
encipherStrRec n xs | isAlpha(head xs) == True = encipher n (head xs) : encipherStrRec n (tail xs)
                    | isDigit(head xs) == True = head xs: encipherStrRec n (tail xs)
                    | otherwise                = encipherStrRec n (tail xs)

encipherStr :: Int -> String -> String
encipherStr n [] = []
encipherStr n xs = encipherStrRec n (normalize xs)

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey [] = []
reverseKey xs = ((snd(head xs)),(fst(head xs))) : reverseKey(tail xs)

-- 9.
decipher :: Int -> Char -> Char
decipher n a | ord a - n >= 65 = chr(ord a - n)
             | otherwise       = chr(ord a - n + 26) 

decipherStr :: Int -> String -> String
decipherStr n [] = []
decipherStr n xs | isAlpha(head xs) == True = decipher n (head xs) : decipherStr n (tail xs)
                 | isDigit(head xs) == True = head xs: decipherStr n (tail xs)
                 | isSpace(head xs) == True = " " ++ decipherStr n (tail xs)

-- 10.

prop_cipher :: Int -> String -> Property
prop_cipher n xs = (0 <= n && n < 26) ==> decipherStr n (encipherStr n xs) ==normalize xs

-- 11.
contains :: String -> String -> Bool
contains xs ys = isInfixOf ys xs

-- 12.

candidates :: String -> [(Int, String)]
candidates xs = candidatesRec xs 1


candidatesRec :: String -> Int -> [(Int, String)]
candidatesRec [] n = []
candidatesRec xs 27 = []
candidatesRec xs n = if contains (decipherStr n xs) "THE" || contains (decipherStr n xs) "AND" then [(n,decipherStr n xs)] ++ candidatesRec xs (n+1)
                     else candidatesRec xs (n+1)


-- Optional Material

-- 13.
splitEachFive :: String -> [String]
splitEachFive xs = take 5 xs : splitEachFive drop 5 xs

-- 14.
prop_transpose :: String -> Bool
prop_transpose = undefined

-- 15.
encrypt :: Int -> String -> String
encrypt = undefined

-- 16.
decrypt :: Int -> String -> String
decrypt = undefined

-- Challenge (Optional)

-- 17.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 18
freqDecipher :: String -> [String]
freqDecipher = undefined