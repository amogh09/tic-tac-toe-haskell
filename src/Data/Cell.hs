module Data.Cell (Cell(..), cellChar) where

data Cell = CellX
          | CellO
          | CellEmpty
          deriving (Show, Eq, Enum, Bounded)

cellChar :: Cell -> Char
cellChar CellX     = 'X'
cellChar CellO     = 'O'
cellChar CellEmpty = '.'
