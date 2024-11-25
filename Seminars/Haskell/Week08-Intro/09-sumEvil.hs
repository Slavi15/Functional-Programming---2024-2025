isEvil :: Int -> Bool
isEvil n = isEvil' n 0
    where
        isEvil' :: Int -> Int -> Bool
        isEvil' 0 count = (count `mod` 2 == 0)
        isEvil' x count = 
            let lastDigit :: Int
                lastDigit = x `mod` 2
            in
                if lastDigit `mod` 2 /= 0
                    then isEvil' (x `div` 2) (count + 1)
                    else isEvil' (x `div` 2) count

sumEvil :: Int -> Int -> Int
sumEvil a b = foldr (\x acc -> if (isEvil x) then (acc + x) else acc) 0 [a .. b]