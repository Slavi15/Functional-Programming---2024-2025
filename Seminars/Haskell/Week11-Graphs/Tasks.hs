type Node = Int
type Graph = [(Node, [Node])]

extractNeighbors :: Graph -> Node -> [Node]
extractNeighbors g u = snd $ head $ filter (\(node, _) -> node == u) g

removeNeighbors :: Graph -> Node -> Graph
removeNeighbors g u = filter (\(node, _) -> node /= u) g

-- >>> extractNeighbors graph 1
-- [2,4]

-- >>> removeNeighbors graph 1
-- [(2,[1,3]),(3,[2,4]),(4,[1,3])]

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

-- >>> pathsFrom graph 2 1
-- [[1,2,3],[1,3,2],[1,3,4],[1,4,3]]

-- >>> pathsFrom graph 2 2
-- [[2,1,3],[2,1,4],[2,3,1],[2,3,4]]

-- >>> paths graph 2
-- [[1,2,3],[1,3,2],[1,3,4],[1,4,3],[2,1,3],[2,1,4],[2,3,4],[3,2,1],[3,4,1],[4,1,2],[4,1,3],[4,3,2]]

isReachable :: Graph -> Node -> Node -> Bool
isReachable g u v = isReachable' g u v []
    where
        isReachable' :: Graph -> Node -> Node -> [Node] -> Bool
        isReachable' g u v visited
            | u == v = True
            | otherwise = any (\v' -> isReachable' (removeNeighbors g u) v' v (u : visited)) (filter (`notElem` visited) nodeNeighbors)

            where
                nodeNeighbors :: [Node]
                nodeNeighbors = extractNeighbors g u

graph :: Graph
graph = [(1, [2, 3, 4]), (2, [1, 3]), (3, [1, 2, 4]), (4, [1, 3])]

-- >>> isReachable graph 2 4
-- True