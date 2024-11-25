isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime k
    | k > 1 = null [x | x <- [2 .. (isqrt k)], k `mod` x == 0]
    | otherwise = False