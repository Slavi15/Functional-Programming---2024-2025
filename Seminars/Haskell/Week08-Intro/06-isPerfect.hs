isPerfect :: Int -> Bool
isPerfect n = isPerfect' (n - 1) 0
    where
        isPerfect' :: Int -> Int -> Bool
        isPerfect' 0 sum = (sum == n)
        isPerfect' x sum
            | n `mod` x == 0 = isPerfect' (x - 1) (sum + x)
            | otherwise = isPerfect' (x - 1) sum

isPerfect'' :: Int -> Bool
isPerfect'' n = sum [x | x <- [1 .. (n - 1)], n `mod` x == 0] == n
