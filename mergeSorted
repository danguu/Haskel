mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted (x:xs) (y:ys)
    | x <= y    = x : mergeSorted xs (y:ys)
    | otherwise = y : mergeSorted (x:xs) ys
