invertTuples :: [(a, b)] -> [(b, a)]
invertTuples xs = [(y, x) | (x, y) <- xs]
