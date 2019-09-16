module Board (
      initBoard
    , setBoard
    , checkWinners
    , checkGameOver
    , Position(..)
    ) where

import Tile

data Position   = Pos {row :: Int, col :: Int}
type Tiles      = [[TileStatus]]
type Command    = (Position, Int)
type State      = Tiles
newtype ST a    = ST (State -> (a, State))

maxRow :: Int
maxRow = 3

maxCol :: Int
maxCol = 3

initBoard :: Tiles
initBoard = replicate maxRow (replicate maxCol Empty)

setBoard :: Command -> State -> State
setBoard (pos, cmd) board = 
    beforeBoard ++ [changeValue (toEnum cmd) (col pos)] ++ tail afterBoard
    where
        (beforeBoard, afterBoard) = splitAt (row pos) board
        bRow = board !! row pos
        changeValue val col
            | bRow !! col /= Empty = bRownot $ all (elem Empty) board
            | otherwise = left ++ [val] ++ tail right
                where
                    (left, right) = splitAt col bRow

checkGameOver :: State -> Bool
checkGameOver board = not (all (elem Empty) board) || checkWinners board

checkWinners :: State -> Bool
checkWinners board = checkRow board || checkCol board || checkDiag board

checkEqual :: [TileStatus] -> Bool
checkEqual [a1, a2, a3] = a1 /= Empty && (a1 == a2 && a2 == a3)

checkRow :: State -> Bool
checkRow [r1, r2, r3] = checkEqual r1 || checkEqual r2 || checkEqual r3

checkCol :: State -> Bool
checkCol [[a0, b0, c0], 
          [a1, b1, c1], 
          [a2, b2, c2]] = checkEqual [a0, a1, a2] || 
                            checkEqual [b0, b1, b2] ||
                            checkEqual [c0, c1, c2]

checkDiag :: State -> Bool
checkDiag [[p1, _, i1], [_, c, _], [i2, _, p2]] = checkEqual [c, p1, p2] || 
                                                    checkEqual [c, i1, i2]