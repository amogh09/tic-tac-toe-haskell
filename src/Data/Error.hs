module Data.Error (Error(..)) where

import           Data.Point (Point)

data Error = PointOutOfBoard Point
           | PointOccupied Point
           | InvalidInput String
           | InvalidGridSize (Int,Int)
           deriving (Show, Eq)
