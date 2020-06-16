module Data.Move (Move(Move), unmove) where

import           Data.Cell  (Cell)
import           Data.Point (Point)

newtype Move = Move
  { unmove :: (Point, Cell)
  } deriving Show
