myGCD :: Int -> Int -> Int
myGCD a b
    | b == 0 = a
    | otherwise = myGCD b (a `mod` b)