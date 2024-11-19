module TicTacToe where

import Matrix

data Marker = X | O
  deriving (Eq, Show)

type Spot = Maybe Marker

type Board = Matrix Spot

data Result = Full | HasEmpty | Wins Marker
  deriving (Show)

infixr 2 `join`

join :: Result -> Result -> Result
join Full Full = Full
join (Wins X) _ = Wins X
join _ (Wins X) = Wins X
join (Wins O) _ = Wins O
join _ (Wins O) = Wins O
join HasEmpty _ = HasEmpty
join _ HasEmpty = HasEmpty

checkThreeSpots :: Thrice Spot -> Result
checkThreeSpots f =
  let spots = [f Zero, f One, f Two]
  in case () of
    _ | all (== Just X) spots -> Wins X
    _ | all (== Just O) spots -> Wins O
    _ | any (== Nothing) spots -> HasEmpty
    _ -> Full

winnerRows :: Board -> Result
winnerRows board =
  let rows = [checkThreeSpots (getRow i board) | i <- [Zero, One, Two]]
  in foldl join HasEmpty rows

winnerCols :: Board -> Result
winnerCols board =
  let cols = [checkThreeSpots (getCol i board) | i <- [Zero, One, Two]]
  in foldl join HasEmpty cols

winnerDiags :: Board -> Result
winnerDiags board =
  let diags = [checkThreeSpots (getDiag board), checkThreeSpots (getOtherDiag board)]
  in foldl join HasEmpty diags

winner :: Board -> Result
winner board = 
  (winnerRows board) `join` (winnerCols board) `join` (winnerDiags board)

emptySpots :: Board -> [(Three, Three)]
emptySpots (MkMatrix board) =
  [ (i, j) | i <- [Zero, One, Two], j <- [Zero, One, Two], (board i j) == Nothing ]
