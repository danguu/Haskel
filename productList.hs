productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs
