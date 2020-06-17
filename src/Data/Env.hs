{-# LANGUAGE FlexibleContexts #-}

module Data.Env
  ( EnvError
  , Env
  , getWriteH
  , getReadH
  , UserEnv (..)
  , envStrategy
  , mkEnv
  , stdioEnv
  , mkIOEnv
  , isSinglePlayer
  ) where

import           Control.Monad.Reader (MonadReader, ask)
import           Data.Strategy        (Strategy)
import           System.IO            (Handle, stdin, stdout)

data Env = Env
  { _ioEnv   :: IOEnv
  , _userEnv :: UserEnv
  }

data IOEnv = IOEnv
  { _readHandle  :: Handle
  , _writeHandle :: Handle
  }

data UserEnv = UserEnv
  { _isSinglePlayer :: Bool
  , _strategy       :: Strategy
  }

data EnvError = NoStrategyForMultiplayer deriving (Show,Eq)

mkEnv :: IOEnv -> UserEnv -> Env
mkEnv io ue = Env io ue

mkIOEnv :: Handle -> Handle -> IOEnv
mkIOEnv rh wh = IOEnv rh wh

stdioEnv :: IOEnv
stdioEnv = IOEnv stdin stdout

getReadH :: MonadReader Env m => m Handle
getReadH = _readHandle <$> _ioEnv <$> ask

getWriteH :: MonadReader Env m => m Handle
getWriteH = _writeHandle <$> _ioEnv <$> ask

envStrategy :: Env -> Strategy
envStrategy = _strategy . _userEnv where

isSinglePlayer :: Env -> Bool
isSinglePlayer = _isSinglePlayer . _userEnv
