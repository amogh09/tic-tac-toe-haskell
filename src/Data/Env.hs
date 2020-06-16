{-# LANGUAGE FlexibleContexts #-}

module Data.Env
  ( Env, stdenv, getReadH, getWriteH, mkEnv
  , strategyEnv
  ) where

import           Control.Monad.Reader (MonadReader, ask)
import           Data.Strategy        (StrategyConf)
import           System.IO            (Handle, stdin, stdout)

data Env = Env
  { _readHandle  :: Handle
  , _writeHandle :: Handle
  , _strategyEnv :: StrategyConf
  }

mkEnv :: Handle -> Handle -> StrategyConf -> Env
mkEnv rh wh stEnv = Env rh wh stEnv

stdenv :: StrategyConf -> Env
stdenv stEnv = Env stdin stdout stEnv

getReadH :: MonadReader Env m => m Handle
getReadH = _readHandle <$> ask

getWriteH :: MonadReader Env m => m Handle
getWriteH = _writeHandle <$> ask

strategyEnv :: Env -> StrategyConf
strategyEnv = _strategyEnv
