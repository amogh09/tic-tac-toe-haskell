{-# LANGUAGE FlexibleContexts #-}

module Data.Env (Env, stdenv, getReadH, getWriteH, mkEnv) where

import           Control.Monad.Reader (MonadReader, ask)
import           System.IO            (Handle, stdin, stdout)

data Env = Env
  { _readHandle  :: Handle
  , _writeHandle :: Handle
  }

mkEnv :: Handle -> Handle -> Env
mkEnv rh wh = Env rh wh

stdenv :: Env
stdenv = Env stdin stdout

getReadH :: MonadReader Env m => m Handle
getReadH = _readHandle <$> ask

getWriteH :: MonadReader Env m => m Handle
getWriteH = _writeHandle <$> ask
