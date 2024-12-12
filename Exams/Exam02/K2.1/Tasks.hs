isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime k
    | k > 1 = null [x | x <- [2 .. (isqrt k)], k `mod` x == 0]
    | otherwise = False

-- >>> isPrime 7
-- True

-- >>> isPrime 8
-- False

areAllDigitsPrime :: Integer -> Bool
areAllDigitsPrime n = areAllDigitsPrime' n 1
    where
        areAllDigitsPrime' :: Integer -> Integer -> Bool
        areAllDigitsPrime' 0 _ = True
        areAllDigitsPrime' x pos
            | isPrime pos && not (isPrime digit) = False
            | otherwise = areAllDigitsPrime' (x `div` 10) (pos + 1)

            where
                digit :: Integer
                digit = x `mod` 10

isSuperPrime :: Integer -> Bool
isSuperPrime n = isPrime n && areAllDigitsPrime n

allSuperPrime :: Integer -> Integer -> [Integer]
allSuperPrime a b = [x | x <- [a .. b], isSuperPrime x]

-- >>> isSuperPrime 123
-- False
-- >>> isSuperPrime 79
-- True
-- >>> isSuperPrime 571
-- True
-- >>> isSuperPrime 1721
-- True
-- >>> isSuperPrime 97
-- False

-- >>> allSuperPrime 1 10
-- [2,3,5,7]

type Node = Int
type Graph = [(Node, [Node])]

extractNeighbors :: Graph -> Node -> [Node]
extractNeighbors g u = snd $ head $ filter (\(node, _) -> node == u) g

removeNeighbors :: Graph -> Node -> Graph
removeNeighbors g u = filter (\(node, _) -> node /= u) g

pathsFrom :: Graph -> Int -> Node -> [[Node]]
pathsFrom g k node = pathsFrom' g k node 0 [] []
    where
        pathsFrom' :: Graph -> Int -> Node -> Int -> [Node] -> [Node] -> [[Node]]
        pathsFrom' g k node pathLength paths visited
            | k == pathLength = [paths ++ [node]]
            | otherwise = concat [pathsFrom' (removeNeighbors g node) k u (pathLength + 1) (paths ++ [node]) (node : visited) | u <- filter (`notElem` visited) nodeNeighbors]

            where
                nodeNeighbors :: [Node]
                nodeNeighbors = extractNeighbors g node

paths :: Graph -> Node -> [[Node]]
paths g k = concatMap (\node -> pathsFrom g k node) nodes
    where
        nodes = map fst g

graph :: Graph
graph = [(1, [2, 3, 4]), (2, [1, 3]), (3, [1, 2, 4]), (4, [1, 3])]

-- >>> extractNeighbors graph 1
-- [2,4]

-- >>> removeNeighbors graph 1
-- [(2,[1,3]),(3,[2,4]),(4,[1,3])]

-- >>> pathsFrom graph 2 1
-- [[1,2,3],[1,3,2],[1,3,4],[1,4,3]]

-- >>> pathsFrom graph 2 2
-- [[2,1,3],[2,1,4],[2,3,1],[2,3,4]]

-- >>> paths graph 2
-- [[1,2,3],[1,3,2],[1,3,4],[1,4,3],[2,1,3],[2,1,4],[2,3,4],[3,2,1],[3,4,1],[4,1,2],[4,1,3],[4,3,2]]
