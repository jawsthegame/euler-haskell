next :: (Num a, Ord a) => [a] -> [a]
next (x:xs)
  | last xs < 4000000 = next ([x] ++ xs ++ [sum (take 2 (reverse ([x] ++ xs)))])
  | otherwise         = [x] ++ init xs

solve = sum [x | x <- next [1,2], x `mod` 2 == 0]
