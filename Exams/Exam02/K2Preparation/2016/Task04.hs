{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import Data.List (maximumBy)
import Data.Ord (comparing)

type Graph = [[Int]]

maxPath :: Graph -> Int -> [Int]
maxPath g u = reverse $ maxPath' g u []
    where
        maxPath' :: Graph -> Int -> [Int] -> [Int]
        maxPath' g u result
            | u `elem` result = result
            | null neighbours = result
            | otherwise = maximumBy (comparing length) paths

            where
                neighbours = extractNeighbours u g
                paths = [maxPath' (removeNeighbours u g) n (u : result) | n <- neighbours]

extractNeighbours :: Int -> Graph -> [Int]
extractNeighbours u g = tail $ head $ filter (\x -> head x == u) g

removeNeighbours :: Int -> Graph -> Graph
removeNeighbours u = filter (\x -> head x /= u)

graph :: Graph
graph = [[1, 2, 4], [2, 3], [3, 2], [4]]

-- >>> maxPath graph 1
-- [1,2,3]

-- >>> removeNeighbours 1 graph
-- [[2,3],[3,2],[4]]

-- >>> extractNeighbours 1 graph
-- [2,4]
