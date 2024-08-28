maxList :: [Int] -> Int
maxList [] = error "Lista vacía"
maxList [x] = x
maxList (x:xs) = max x (maxList xs)
