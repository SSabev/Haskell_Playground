sum :: [Integer] -> [Integer]
sum [] = 0
sum (x:xs) = x + sum xs