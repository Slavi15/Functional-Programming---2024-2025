nub :: (Eq a) => [a] -> [a]
nub xs = nub' xs []
    where
        nub' :: (Eq a) => [a] -> [a] -> [a]
        nub' [] visited = visited
        nub' (x : xs) visited
            | x `elem` visited = nub' xs visited
            | otherwise = nub' xs (visited ++ [x])

group :: (Eq a) => [a] -> [[a]]
group = groupBy (==)

groupBy :: (Eq a) => (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f (x : xs) = 
    let (curr, rest) = span (f x) xs
    in (x : curr) : groupBy f rest

sort :: (Ord a) => [a] -> [a]
sort = sortBy (<)

sortBy :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [x] = [x]
sortBy f xs = merge f lhs rhs
    where
        mid = length xs `div` 2
        lhs = sortBy f (take mid xs)
        rhs = sortBy f (drop mid xs)

merge :: (Ord a) => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] ys = ys
merge f (x : xs) (y : ys)
    | f x y = x : merge f xs (y : ys)
    | otherwise = y : merge f (x : xs) ys

minimum' :: (Foldable t, Ord a) => t a -> a
minimum' = minimumBy' compare

minimumBy' :: (Foldable t, Ord a) => (a -> a -> Ordering) -> t a -> a
minimumBy' _ xs | null xs = error "empty"
minimumBy' f xs = foldl1 (chooseMin f) xs
    where
        chooseMin :: (a -> a -> Ordering) -> a -> a -> a
        chooseMin cmp x y = case cmp x y of
            LT -> x
            _ -> y

maximum' :: (Foldable t, Ord a) => t a -> a
maximum' = maximumBy' compare

maximumBy' :: (Foldable t, Ord a) => (a -> a -> Ordering) -> t a -> a
maximumBy' _ xs | null xs = error "empty"
maximumBy' f xs = foldl1 (chooseMax f) xs
    where
        chooseMax :: (a -> a -> Ordering) -> a -> a -> a
        chooseMax cmp x y = case cmp x y of
            GT -> x
            _ -> y

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose xss = map head xss : transpose (map tail xss)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
