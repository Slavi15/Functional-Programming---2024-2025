myGCD :: Int -> Int -> Int
myGCD a b
    | b == 0 = a
    | otherwise = myGCD b (a `mod` b)

myLCM :: Int -> Int -> Int
myLCM a b = (div a (myGCD a b)) * b