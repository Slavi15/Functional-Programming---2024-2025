import Data.List (nub)

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

genPowers :: Integer -> Integer -> [Integer]
genPowers k t = nub $ 1 : genPowers' [k^x | x <- [1 ..]] [t^x | x <- [1 ..]]
    where
        genPowers' (l : lstream) (r : rstream) =
            if l < r
                then l : genPowers' lstream (merge (map (* l) (r : rstream)) (r : rstream))
                else r : genPowers' (merge (map (* r) (l : lstream)) (l : lstream)) rstream

-- >>> take 10 $ genPowers 2 3
-- [1,2,3,4,6,8,9,12,16,18]
