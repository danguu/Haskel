factorialFoldl :: Int -> Int
factorialFoldl n = foldl (*) 1 [1..n]
