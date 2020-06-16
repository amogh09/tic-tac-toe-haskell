module Data.State
  ( GameState, strategyState, mkGameState
  , putStrategyState
  ) where

import           Data.Strategy (StrategyState, mkStrategyState)
import           System.Random (StdGen)

data GameState = GameState
  { _strategyState :: StrategyState
  }

strategyState :: GameState -> StrategyState
strategyState = _strategyState

mkGameState :: StdGen -> GameState
mkGameState g = GameState $ mkStrategyState g

putStrategyState :: StrategyState -> GameState -> GameState
putStrategyState ss gs = gs { _strategyState = ss }
