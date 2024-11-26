isPrime :: Int -> Bool
isPrime k
    | k > 1 = null [x | x <- [2 .. floor (sqrt (fromIntegral k))], k `mod` x == 0]
    | otherwise = False