{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}

module Data.Strategy
  ( nextMove, runStrategy, GameStrategy
  , StrategyConf, StrategyState, StrategyError
  , mkStrategyState, mkStrategyConf, StrategyCell (StrategyCellX)
  , StrategyDifficulty (Easy)
  ) where

import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State  (MonadState, StateT, get, put, runStateT)
import           Data.Board           (Board, boardFull, emptyPoints)
import           Data.Cell            (Cell (CellO, CellX))
import           Data.Move            (Move (Move))
import           Data.Point           (Point)
import           System.Random        (RandomGen, StdGen, randomR)

newtype Strategy r g e a = Strategy
  { unStrategy :: ReaderT r (StateT g (Either e)) a
  } deriving
  ( Functor, Applicative, Monad, MonadState g, MonadError e, MonadReader r )

runStrategy :: r -> g -> Strategy r g e a -> Either e (a,g)
runStrategy r g = flip runStateT g . flip runReaderT r . unStrategy

type GameStrategy = Strategy StrategyConf StrategyState StrategyError

data StrategyState = StrategyState
  { _strategyState :: StdGen
  }

mkStrategyState :: StdGen -> StrategyState
mkStrategyState g = StrategyState g

data StrategyError = StrategyBoardFullError Board
                     deriving (Show, Eq)

data StrategyDifficulty = Easy
                        deriving Show

data StrategyCell = StrategyCellX
                  | StrategyCellO
                  deriving Show

unStrategyCell :: StrategyConf -> Cell
unStrategyCell StrategyConf { strategyCell=StrategyCellX } = CellX
unStrategyCell StrategyConf { strategyCell=StrategyCellO } = CellO

data StrategyConf = StrategyConf
  { difficulty   :: StrategyDifficulty
  , strategyCell :: StrategyCell
  } deriving Show

mkStrategyConf :: StrategyDifficulty -> StrategyCell -> StrategyConf
mkStrategyConf diff cell = StrategyConf diff cell

nextMove :: Board -> GameStrategy Move
nextMove b = do
  cnf <- ask
  let f = chooseNextPointFun cnf
  Move . (, unStrategyCell cnf) <$> f b

chooseNextPointFun :: StrategyConf -> NextPointFun
chooseNextPointFun StrategyConf { difficulty=Easy, strategyCell=StrategyCellX } =
  easyStrategyNextPoint
chooseNextPointFun StrategyConf { difficulty=Easy, strategyCell=StrategyCellO } =
  easyStrategyNextPoint

type NextPointFun = Board -> GameStrategy Point

easyStrategyNextPoint :: NextPointFun
easyStrategyNextPoint b | boardFull b = throwError $ StrategyBoardFullError b
easyStrategyNextPoint b = do
  st    <- get
  let
    (i,g) = randomR (0,length ps - 1) (_strategyState st)
    ps    = emptyPoints b
  put $ st { _strategyState = g }
  pure $ ps !! i
