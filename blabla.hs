oddsRec :: [Integer] -> [Integer]
oddsRec [] = []
oddsRec (x:xs) | odd x = x: oddsRec xs
               | otherwise = oddsRec xs