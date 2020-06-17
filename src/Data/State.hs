module Data.State
  ( GameState, strategyState, mkGameState
  , putStrategyState, ioState
  ) where

import           System.Random (StdGen, getStdGen)

data GameState = GameState
  { _strategyState :: StdGen
  }

strategyState :: GameState -> StdGen
strategyState = _strategyState

mkGameState :: StdGen -> GameState
mkGameState g = GameState g

ioState :: IO GameState
ioState = GameState <$> getStdGen

putStrategyState :: StdGen -> GameState -> GameState
putStrategyState ss gs = gs { _strategyState = ss }
