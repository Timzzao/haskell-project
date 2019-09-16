module Tile (
     TileStatus(..)
    ) where

data TileStatus = Empty | X | O deriving (Eq, Enum)

instance Show TileStatus where
    show Empty = show " "
    show X = show "X"
    show O = show "O"

