{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Game (launchGame, newGame, rungame, GameRunner) where

import           Control.Applicative    ((<|>))
import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State    (MonadState, StateT, get, modify, put,
                                         runStateT)
import           Data.Board             (Board, BoardError (PointOccupied, PointOutOfBoard),
                                         boardFull, emptyBoard, extractCell,
                                         pointInBoard, updateBoard,
                                         winnerExists)
import           Data.Cell              (Cell (CellEmpty, CellO, CellX),
                                         cellChar)
import           Data.Char              (digitToInt, isDigit, toLower)
import           Data.Env               (Env, getReadH, getWriteH, stdenv,
                                         strategyEnv)
import           Data.Error             (Error (GameBoardError, GameStrategyError, InvalidInput))
import           Data.Move              (Move (Move))
import           Data.Point             (Point (Point))
import           Data.State             (GameState, mkGameState,
                                         putStrategyState, strategyState)
import           Data.Strategy          (GameStrategy,
                                         StrategyCell (StrategyCellX),
                                         StrategyDifficulty (Easy),
                                         StrategyState, mkStrategyConf,
                                         nextMove, runStrategy)
import           System.IO              (BufferMode (NoBuffering), hGetLine,
                                         hPutStr, hPutStrLn, hSetBuffering,
                                         stdout)
import           System.Random          (RandomGen, getStdGen)

newtype Game r s e m a = Game
  { ungame :: ReaderT r (StateT s (ExceptT e m)) a
  } deriving
  ( Functor, Applicative, Monad, MonadError e, MonadIO, MonadReader r
  , MonadState s)

rungame :: r -> s -> Game r s e m a -> m (Either e (a,s))
rungame r s = runExceptT . flip runStateT s . flip runReaderT r . ungame

type GameXO m = Game Env GameState Error m
type GameRunner m = Board -> GameXO m ()

printBoard :: MonadIO m => Board -> GameXO m Board
printBoard b = gamePutStrLn (show b) *> pure b

gamePutStr :: (MonadIO m) => String -> GameXO m ()
gamePutStr xs = getWriteH >>= liftIO . flip hPutStr xs

gamePutStrLn :: (MonadIO s) => String -> GameXO s ()
gamePutStrLn xs = getWriteH >>= liftIO . flip hPutStrLn xs

gameGetLine :: MonadIO m => GameXO m String
gameGetLine = getReadH >>= liftIO . hGetLine

readPoint :: MonadError Error m => String -> m Point
readPoint [x,y] | isDigit x && isDigit y =
  let p = Point (digitToInt x, digitToInt y)
  in  if pointInBoard p then pure p else throwError $ GameBoardError $ PointOutOfBoard p
readPoint xs = throwError $ InvalidInput xs

declareResult :: MonadIO m => Cell -> GameXO m ()
declareResult CellEmpty = gamePutStrLn "It's a draw"
declareResult c         = gamePutStrLn $ "Winner is " <> [cellChar c]

getMove :: MonadIO m => Bool -> Board -> GameXO m Point
getMove isx b = act `catchError` \e -> printError e *> getMove isx b
  where
    c   = if isx then CellX else CellO
    act = do
      gamePutStr $ "Enter move for " <> [cellChar c] <> ": "
      l  <- gameGetLine
      p  <- readPoint l
      c' <- either (throwError . GameBoardError) pure $ extractCell p b
      if c' /= CellEmpty
        then throwError $ GameBoardError $ PointOccupied p
        else pure p

cellEmptyIfBoardFull :: Board -> Maybe Cell
cellEmptyIfBoardFull b = if boardFull b then Just CellEmpty else Nothing

playGame :: MonadIO m => Bool -> GameRunner m
playGame isx b =
      getMove isx b
  >>= liftBoard . updateBoard b . Move . (,if isx then CellX else CellO)
  >>= printBoard
  >>= \b' ->
        maybe
          (playGame (not isx) b')
          declareResult
          (winnerExists b' <|> cellEmptyIfBoardFull b')

liftBoard :: Monad m => Either BoardError a -> GameXO m a
liftBoard = either (throwError . GameBoardError) pure

liftStrategy :: Monad m => GameStrategy a -> GameXO m (a, StrategyState)
liftStrategy s = do
  c <- asks strategyEnv
  g <- strategyState <$> get
  either (throwError . GameStrategyError) pure $ runStrategy c g s

-- |Computer's turn when it's playing as X.
singlePlayerGameX :: (MonadIO m) => GameRunner m
singlePlayerGameX b = do
  gamePutStrLn "Computer's turn"
  (mv,st) <- liftStrategy $ nextMove b
  modify (putStrategyState st)
  b'      <- liftBoard $ updateBoard b mv
  printBoard b'
  maybe
    (singlePlayerGameXPT b')
    declareResult
    (winnerExists b' <|> cellEmptyIfBoardFull b')

-- |Player's turn when playing as O.
singlePlayerGameXPT :: (MonadIO m) => GameRunner m
singlePlayerGameXPT b = do
  p  <- getMove False b
  b' <- liftBoard . updateBoard b . Move . (,CellO) $ p
  _  <- printBoard b'
  maybe
    (singlePlayerGameX b')
    declareResult
    (winnerExists b' <|> cellEmptyIfBoardFull b')

printError :: (MonadIO m) => Error -> GameXO m ()
printError (GameBoardError (PointOccupied p)) =
  gamePutStrLn $ "Error: Cell " <> show p <> " is already occupied."
printError e = gamePutStrLn $ "Error: " <> show e

playAgain :: (MonadIO m) => GameRunner m -> GameXO m ()
playAgain gr = do
  gamePutStr "Do you want to play again? [Y/N]: "
  ans <- gameGetLine
  if (toLower <$> ans) `elem` ["y", "yes"]
    then newGame gr
    else gamePutStrLn "Bye!"

newGame :: (MonadIO m) => GameRunner m -> GameXO m ()
newGame gr = do
  gamePutStrLn "Starting new game"
  b <- printBoard emptyBoard
  (gr b *> playAgain gr) `catchError` printError

launchGame :: IO ()
launchGame = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to tic-tac-toe!"
  putStr "Press Return key to start a new game."
  getLine
  g   <- getStdGen
  res <- rungame
    (stdenv $ mkStrategyConf Easy StrategyCellX)
    (mkGameState g)
    (newGame singlePlayerGameX)
  either print (\_ -> pure ()) res
