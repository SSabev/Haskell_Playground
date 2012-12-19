squaresRec :: [Integer] -> [Integer]
squaresRec [] = []
squaresRec (x:xs) = x*x : squaresRec xs