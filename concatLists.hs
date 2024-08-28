concatLists :: [a] -> [a] -> [a]
concatLists [] ys = ys
concatLists (x:xs) ys = x : concatLists xs ys
