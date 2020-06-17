{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types ( GameXO, rungame, Game ) where

import           Control.Monad.Except   (ExceptT, MonadError, mapExceptT,
                                         runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, mapReaderT,
                                         runReaderT)
import           Control.Monad.State    (MonadState, StateT, mapStateT,
                                         runStateT)
import           Data.Bifunctor         (Bifunctor, bimap)
import           Data.Env               (Env)
import           Data.Error             (Error)
import           Data.State             (GameState)

newtype Game r s m e a = Game
  { ungame :: ReaderT r (StateT s (ExceptT e m)) a
  } deriving
  ( Functor, Applicative, Monad, MonadError e, MonadIO, MonadReader r
  , MonadState s)

instance Functor m => Bifunctor (Game r s m) where
  bimap f g = Game . mapReaderT (mapStateT $ mapExceptT $ fmap (bimap f g')) . ungame
    where g' (x,s) = (g x, s)

rungame :: r -> s -> Game r s m e a -> m (Either e (a,s))
rungame r s = runExceptT . flip runStateT s . flip runReaderT r . ungame

type GameXO m = Game Env GameState m Error
