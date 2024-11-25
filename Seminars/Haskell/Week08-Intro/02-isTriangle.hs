isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c
    | (a + b) > c && (a + c) > b && (b + c) > a = True
    | otherwise = False