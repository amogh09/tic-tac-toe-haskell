{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( GameXO
  , rungame
  , Game
  , AppIO
  , gamePutStr
  , gamePutStrLn
  , gameGetLine
  , printBoard
  , PointGetter
  ) where

import           Control.Monad.Except      (ExceptT, MonadError, mapExceptT,
                                            runExceptT)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, mapReaderT,
                                            runReaderT)
import           Control.Monad.State       (MonadState, StateT, mapStateT,
                                            runStateT)
import           Control.Monad.Trans.Class (lift)
import           Data.Bifunctor            (Bifunctor, bimap)
import           Data.Board                (Board, Point)
import           Data.Env                  (Env)
import           Data.Error                (Error)
import           Data.State                (GameState)

newtype Game r s m e a = Game
  { ungame :: ReaderT r (StateT s (ExceptT e m)) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError e
  , MonadIO
  , MonadReader r
  , MonadState s
  )

instance Functor m => Bifunctor (Game r s m) where
  bimap f g = Game . mapReaderT (mapStateT $ mapExceptT $ fmap (bimap f g')) . ungame
    where g' (x,s) = (g x, s)

rungame :: r -> s -> Game r s m e a -> m (Either e (a,s))
rungame r s = runExceptT . flip runStateT s . flip runReaderT r . ungame

type PointGetter m = (Board -> GameXO m Point)

type GameXO m = Game Env GameState m Error

class Monad m => AppIO m where
  gamePutStr :: String -> m ()
  gamePutStrLn :: String -> m ()
  gameGetLine :: m String

  printBoard :: Board -> m Board
  printBoard b = gamePutStrLn (show b) *> pure b

instance AppIO IO where
  gamePutStr = putStr
  gamePutStrLn = putStrLn
  gameGetLine = getLine

instance (AppIO m) => AppIO (Game r s m e) where
  gamePutStr = Game . lift . lift . lift . gamePutStr
  gamePutStrLn = Game . lift . lift . lift . gamePutStrLn
  gameGetLine = Game . lift . lift . lift $ gameGetLine
  printBoard = Game . lift . lift . lift . printBoard
