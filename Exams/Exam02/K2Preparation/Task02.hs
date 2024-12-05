{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

averageCalculation :: Int -> Int -> Float
averageCalculation x y = fromIntegral x / fromIntegral y

average :: [Int] -> Float
average xs = averageCalculation (sum xs) (length xs)

movingAverage :: [Int] -> Int -> [Float]
movingAverage xs n = [average (take n (drop i xs)) | i <- [0 .. (length xs - n)]]

allAverages :: [Int] -> [[Float]]
allAverages xs = map (\n -> movingAverage xs n) [2 ..]

-- >>> movingAverage [1076,1356,1918,6252,6766,5525] 3
-- [1450.0,3175.3333,4978.6665,6181.0]

-- >>> average [1356,1918,6252]
-- 3175.3333

-- >>> take 4 $ allAverages [1076,1356,1918,6252,6766,5525]
-- [[1216.0,1637.0,4085.0,6509.0,6145.5],[1450.0,3175.3333,4978.6665,6181.0],[2650.5,4073.0,5115.25],[3473.6,4363.4]]
