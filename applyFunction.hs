applyFunction :: (a -> b) -> [a] -> [b]
applyFunction f xs = map f xs
