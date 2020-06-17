{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Strategy
  ( nextPoint
  , StrategyError (StrategyBoardFullError)
  , Strategy
  , strategyEasyX
  ) where

import           Control.Monad.Except (MonadError, throwError)
import           Data.Board           (Board, Point, boardFull, emptyPoints)
import           System.Random        (RandomGen, StdGen, randomR)

data StrategyError = StrategyBoardFullError Board
                     deriving (Show, Eq)

newtype Strategy = Strategy
  { _nextPoint
      :: StdGen
      -> Board
      -> Either StrategyError (Point, StdGen)
  }

nextPoint
  :: (MonadError StrategyError m)
  => Strategy
  -> StdGen
  -> Board
  -> m (Point, StdGen)
nextPoint st g = either throwError pure . _nextPoint st g

strategyEasyX :: Strategy
strategyEasyX = Strategy nextPointEasyX

nextPointEasyX
  :: (RandomGen g, MonadError StrategyError m)
  => g
  -> Board
  -> m (Point,g)
nextPointEasyX _ b | boardFull b = throwError $ StrategyBoardFullError b
nextPointEasyX g b =
  let
    ps     = emptyPoints b
    (i,g') = randomR (0,length ps - 1) g
  in
    pure (ps !! i, g')

