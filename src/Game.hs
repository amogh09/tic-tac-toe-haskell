{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Game (launchGame, newGame, rungame) where

import           Control.Applicative    ((<|>))
import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Data.Board             (Board, boardFull, emptyBoard,
                                         extractCell, pointInBoard, updateBoard,
                                         winnerExists)
import           Data.Cell              (Cell (CellEmpty, CellO, CellX),
                                         cellChar)
import           Data.Char              (digitToInt, isDigit, toLower)
import           Data.Env               (Env, getReadH, getWriteH, stdenv)
import           Data.Error             (Error (InvalidInput, PointOccupied, PointOutOfBoard))
import           Data.Move              (Move (Move))
import           Data.Point             (Point (Point))
import           System.IO              (BufferMode (NoBuffering), hGetLine,
                                         hPutStr, hPutStrLn, hSetBuffering,
                                         stdout)

newtype Game r e m a = Game
  { ungame :: ReaderT r (ExceptT e m) a
  } deriving (Functor, Applicative, Monad, MonadError e, MonadIO, MonadReader r)

rungame :: r -> Game r e m a -> m (Either e a)
rungame r = runExceptT . flip runReaderT r . ungame

type GameXO = Game Env Error IO

printBoard :: Board -> GameXO Board
printBoard b = gamePutStrLn (show b) *> pure b

gamePutStr :: String -> GameXO ()
gamePutStr xs = getWriteH >>= liftIO . flip hPutStr xs

gamePutStrLn :: String -> GameXO ()
gamePutStrLn xs = getWriteH >>= liftIO . flip hPutStrLn xs

gameGetLine :: GameXO String
gameGetLine = getReadH >>= liftIO . hGetLine

readPoint :: MonadError Error m => String -> m Point
readPoint [x,y] | isDigit x && isDigit y =
  let p = Point (digitToInt x, digitToInt y)
  in  if pointInBoard p then pure p else throwError $ PointOutOfBoard p
readPoint xs = throwError $ InvalidInput xs

declareResult :: Cell -> GameXO ()
declareResult CellEmpty = gamePutStrLn "It's a draw"
declareResult c         = gamePutStrLn $ "Winner is " <> [cellChar c]

getMove :: Bool -> Board -> GameXO Point
getMove isx b = act `catchError` \e -> printError e *> getMove isx b
  where
    c   = if isx then CellX else CellO
    act = do
      gamePutStr $ "Enter move for " <> [cellChar c] <> ": "
      l  <- gameGetLine
      p  <- readPoint l
      c' <- extractCell p b
      if c' /= CellEmpty
        then throwError $ PointOccupied p
        else pure p

cellEmptyIfBoardFull :: Board -> Maybe Cell
cellEmptyIfBoardFull b = if boardFull b then Just CellEmpty else Nothing

playGame :: Bool -> Board -> GameXO ()
playGame isx b =
      getMove isx b
  >>= updateBoard b . Move . (,if isx then CellX else CellO)
  >>= printBoard
  >>= \b' ->
        maybe
          (playGame (not isx) b')
          declareResult
          (winnerExists b' <|> cellEmptyIfBoardFull b')

printError :: Error -> GameXO ()
printError (PointOccupied p) =
  gamePutStrLn $ "Error: Cell " <> show p <> " is already occupied."
printError e = gamePutStrLn $ "Error: " <> show e

playAgain :: GameXO ()
playAgain = do
  gamePutStr "Do you want to play again? [Y/N]: "
  ans <- gameGetLine
  if (toLower <$> ans) `elem` ["y", "yes"]
    then newGame
    else gamePutStrLn "Bye!"

newGame :: GameXO ()
newGame = do
  gamePutStrLn "Starting new game"
  b <- printBoard emptyBoard
  (playGame True b *> playAgain) `catchError` printError

launchGame :: IO ()
launchGame =
      hSetBuffering stdout NoBuffering
  *>  putStrLn "Welcome to tic-tac-toe!"
  *>  putStr "Press Return key to start a new game."
  *>  getLine
  *>  rungame stdenv newGame
  >>= either print (\_ -> pure ())
