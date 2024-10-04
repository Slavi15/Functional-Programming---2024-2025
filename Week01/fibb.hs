fibb :: Int -> Int
fibb n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = fibb (n - 1) + fibb (n - 2)