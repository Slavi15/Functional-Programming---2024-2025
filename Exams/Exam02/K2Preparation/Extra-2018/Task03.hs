{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import Data.List (nub)

type Result = (String, String, Int, Int)
type Score = (String, Int, Int, Int)

allTeams :: [Result] -> [String]
allTeams games = allTeams' games []
    where
        allTeams' :: [Result] -> [String] -> [String]
        allTeams' [] teams = nub teams
        allTeams' ((t1, t2, _, _) : games') teams = allTeams' games' (t1 : t2 : teams)

tournament :: [Result]
tournament = [("A", "B", 1, 0), ("B", "C", 4, 1), ("C", "B", 3, 3), ("B", "A", 1, 2), ("A", "C", 0, 1)]

-- >>> allTeams tournament
-- ["A","C","B"]

teamScore :: [Result] -> String -> (Int, Int, Int)
teamScore games team = teamScore' games team (0, 0, 0)
    where
        teamScore' :: [Result] -> String -> (Int, Int, Int) -> (Int, Int, Int)
        teamScore' [] _ res = res
        teamScore' ((t1, t2, g1, g2) : games') team' res@(pts, scored, taken)
            | t1 == team' =
                case () of
                    () | g1 > g2 -> teamScore' games' team' (pts + 3, scored + g1, taken + g2)
                        | g1 < g2 -> teamScore' games' team' (pts, scored + g1, taken + g2)
                        | otherwise -> teamScore' games' team' (pts + 1, scored + g1, taken + g2)
            | t2 == team' =
                case () of
                    () | g1 > g2 -> teamScore' games' team' (pts, scored + g2, taken + g1)
                        | g1 < g2 -> teamScore' games' team' (pts + 3, scored + g2, taken + g1)
                        | otherwise -> teamScore' games' team' (pts + 1, scored + g2, taken + g1)
            | otherwise = teamScore' games' team' res

-- >>> teamScore tournament "A"
-- (6,3,2)

-- >>> teamScore tournament "B"
-- (4,8,7)

-- >>> teamScore tournament "C"
-- (4,5,7)

scoreBoard :: [Result] -> [Score]
scoreBoard games = 
    [(team, pts, goalsScored, goalsTaken) | 
    (team, (pts, goalsScored, goalsTaken)) <- zip teams scores]
    where
        teams :: [String]
        teams = allTeams games

        scores :: [(Int, Int, Int)]
        scores = map (\ team -> teamScore games team) teams

-- >>> scoreBoard tournament
-- [("A",6,3,2),("C",4,5,7),("B",4,8,7)]
