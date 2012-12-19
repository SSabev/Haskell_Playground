-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 11/12 November

import System.IO.Unsafe
import System.Random


-- Importing the keymap module

import KeymapTree 


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalog = Keymap Barcode Item


-- A little test catalog

testDB :: Catalog
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("0105992002535", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProduct :: [(Barcode, Item)] -> Int
longestProduct = maximum.map (length.fst.snd)

formatLine :: Int -> (Barcode, Item) -> String
formatLine x xs = fst xs ++ "..." ++ fst (snd xs) ++ [y | y<-(take (x - length (fst (snd xs))) (replicate x '.'))] ++ "..." ++  snd (snd xs)

getBigNumber :: Catalog -> Int
getBigNumber xs = longestProduct (toList xs)

showCatalog :: Catalog -> String
showCatalog c = unlines ( map ( formatLine n ) xs)
                where xs = toList c
                      n = longestProduct xs
     

-- Exercise 2

getItems :: [Barcode] -> Catalog -> [Item]
getItems bcs cat = foldr (++) [] (map (unpack.func) bcs)
                   where unpack (Just a) = [a]
                         unpack Nothing  = []
                         func bc = get bc cat








-- Input-output ------------------------------------------

readDB :: IO Catalog
readDB = do db <- readFile "database.csv"
            return $ fromList $ map readLine $ lines db

makeDB :: IO ()
makeDB | size theDB >= 0 = putStrLn "done" 

theDB :: Catalog
theDB = unsafePerformIO readDB

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: IO Barcode
getSample = do g <- newStdGen
               return $ fst $ toList theDB !! fst (randomR (0,104650) g)


