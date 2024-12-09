{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

import Data.List (sortBy, minimumBy)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

addToList :: [(String, Integer, Integer)] -> (String, Integer, Integer) -> (String, Integer, Integer) -> [(String, Integer, Integer)]
addToList [] lhs@(_, _, _) rhs@(_, _, _) = [lhs] ++ [rhs]
addToList teams ("", 0, 0) ("", 0, 0) = teams
addToList (entry@(currTeam, currGoals, currPoints) : teams) lhs@(lhsTeam, lhsGoals, lhsPoints) rhs@(rhsTeam, rhsGoals, rhsPoints)
    | currTeam == lhsTeam = (lhsTeam, currGoals + lhsGoals, currPoints + lhsPoints) : addToList teams ("", 0, 0) rhs
    | currTeam == rhsTeam = (rhsTeam, currGoals + rhsGoals, currPoints + rhsPoints) : addToList teams lhs ("", 0, 0)
    | otherwise = entry : addToList teams lhs rhs

fillResults :: [(String, String, Integer, Integer)] -> [(String, Integer, Integer)] -> [(String, Integer, Integer)]
fillResults [] tourResults = tourResults
fillResults ((lhsTeam, rhsTeam, lhsGoals, rhsGoals) : matches) tourResults
    | lhsGoals > rhsGoals = fillResults matches $ addToList tourResults (lhsTeam, lhsGoals - rhsGoals, 3) (rhsTeam, rhsGoals - lhsGoals, 0)
    | lhsGoals < rhsGoals = fillResults matches $ addToList tourResults (lhsTeam, lhsGoals - rhsGoals, 0) (rhsTeam, rhsGoals - lhsGoals, 3)
    | otherwise = fillResults matches $ addToList tourResults (lhsTeam, lhsGoals - rhsGoals, 1) (rhsTeam, rhsGoals - lhsGoals, 1)

sortComparator :: (Ord a) => (String, a, Integer) -> (String, a, Integer) -> Ordering
sortComparator (_, lhsGoals, _) (_, rhsGoals, _)
    | lhsGoals < rhsGoals = GT
    | lhsGoals > rhsGoals = LT
    | otherwise = EQ

minimumComparator :: (Ord a) => (String, Integer, a) -> (String, Integer, a) -> Ordering
minimumComparator (_, _, lhsPoints) (_, _, rhsPoints)
    | lhsPoints < rhsPoints = LT
    | lhsPoints > rhsPoints = GT
    | otherwise = EQ

maxGoalMinPoints :: [(String, String, Integer, Integer)] -> String
maxGoalMinPoints tournament' = first $ head filtered
    where
        filtered :: [(String, Integer, Integer)]
        filtered = filter (\(str, _, _) -> str /= "") increasingPoints

        increasingPoints :: [(String, Integer, Integer)]
        increasingPoints = sortBy minimumComparator decreasingGoals

        decreasingGoals :: [(String, Integer, Integer)]
        decreasingGoals = sortBy sortComparator finalResults

        finalResults :: [(String, Integer, Integer)]
        finalResults = fillResults tournament' results

first :: (String, Integer, Integer) -> String
first (teamName, _, _) = teamName

results :: [(String, Integer, Integer)]
results = []

tournament :: [(String, String, Integer, Integer)]
tournament = [("A", "B", 1, 0), ("B", "C", 4, 1), ("C", "B", 3, 3), ("B", "A", 1, 2), ("A", "C", 0, 1)]

-- "A" => 3 | 1 | 6
-- "B" => 8 | 7 | 4
-- "C" => 5 | 7 | 4

-- >>> maxGoalMinPoints tournament
-- "B"
