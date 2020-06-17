module Data.Error (Error(..)) where

import           Data.Board    (BoardError)
import           Data.Env      (EnvError)
import           Data.Strategy (StrategyError)

data Error = GameBoardError BoardError
           | GameStrategyError StrategyError
           | InvalidInput String
           | GameEnvError EnvError
           deriving (Show, Eq)
