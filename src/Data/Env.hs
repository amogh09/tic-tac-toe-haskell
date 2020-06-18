{-# LANGUAGE FlexibleContexts #-}

module Data.Env
  ( EnvError
  , Env
  , UserEnv (..)
  , envStrategy
  , mkEnv
  , isSinglePlayer
  ) where

import           Data.Strategy (Strategy)

data Env = Env
  { _userEnv :: UserEnv
  }

data UserEnv = UserEnv
  { _isSinglePlayer :: Bool
  , _strategy       :: Strategy
  }

data EnvError = NoStrategyForMultiplayer deriving (Show,Eq)

mkEnv :: UserEnv -> Env
mkEnv ue = Env ue

envStrategy :: Env -> Strategy
envStrategy = _strategy . _userEnv where

isSinglePlayer :: Env -> Bool
isSinglePlayer = _isSinglePlayer . _userEnv
