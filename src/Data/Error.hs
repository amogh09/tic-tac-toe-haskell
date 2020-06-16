module Data.Error (Error(..)) where

import           Data.Board    (BoardError)
import           Data.Point    (Point)
import           Data.Strategy (StrategyError)

data Error = GameBoardError BoardError
           | GameStrategyError StrategyError
           | InvalidInput String
           deriving (Show, Eq)
