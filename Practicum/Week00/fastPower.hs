fastPower :: Int -> Int -> Int
fastPower n k
    | k == 0 = 1
    | even k = let halfPow = fastPower n (div k 2)
                in halfPow * halfPow
    | otherwise = n * fastPower n (k - 1)