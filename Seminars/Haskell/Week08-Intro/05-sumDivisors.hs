sumDivisors :: Int -> Int
sumDivisors n = sumDivisors' n 0
    where
        sumDivisors' :: Int -> Int -> Int
        sumDivisors' 0 sum = sum
        sumDivisors' x sum
            | n `mod` x == 0 = sumDivisors' (x - 1) (sum + x)
            | otherwise = sumDivisors' (x - 1) sum