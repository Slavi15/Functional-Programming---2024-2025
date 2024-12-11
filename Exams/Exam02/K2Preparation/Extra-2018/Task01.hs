racaman :: [Integer]
racaman = 0 : racaman' 0 1 []
    where
        racaman' :: Integer -> Integer -> [Integer] -> [Integer]
        racaman' prev i acc
            | prev > i && (minus `notElem` acc) = minus : racaman' minus (i + 1) (minus : acc)
            | otherwise = plus : racaman' plus (i + 1) (plus : acc)

            where
                plus :: Integer
                plus = prev + i

                minus :: Integer
                minus = prev - i

-- >>> take 10 $ racaman
-- [0,1,3,6,2,7,13,20,12,21]
