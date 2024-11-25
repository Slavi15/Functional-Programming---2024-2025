isDigit :: Int -> Bool
isDigit x
    | (x >= 0) && (x <= 9) = True
    | otherwise = False

countBinaryDigits :: Int -> Int
countBinaryDigits n = countBinaryDigits' n 1
    where
        countBinaryDigits' :: Int -> Int -> Int
        countBinaryDigits' x count
            | (isDigit x) = count
            | otherwise = countBinaryDigits' (x `div` 10) (count + 1)