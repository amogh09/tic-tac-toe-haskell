{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Game
  ( launchGame
  , rungame
  , playGame
  , PointGetter
  , playerPointGetter
  ) where

import           Control.Applicative  ((<|>))
import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.Reader (asks)
import           Control.Monad.State  (get, modify)
import           Data.Bifunctor       (first)
import qualified Data.Board           as B
import           Data.Cell            (Cell (CellEmpty, CellO, CellX), cellChar)
import           Data.Char            (digitToInt, isDigit, toLower)
import           Data.Env             (UserEnv (..), envStrategy,
                                       isSinglePlayer, mkEnv)
import           Data.Error           (Error (..))
import           Data.State           (mkGameState, putStrategyState,
                                       strategyState)
import           Data.Strategy        (nextPoint, strategyEasyX)
import           System.IO            (BufferMode (NoBuffering), hSetBuffering,
                                       stdout)
import           System.Random        (getStdGen)
import           Types                (AppIO, GameXO, PointGetter, gameGetLine,
                                       gamePutStr, gamePutStrLn, printBoard,
                                       rungame)

readPoint :: Monad m => String -> GameXO m B.Point
readPoint [x,y] | isDigit x && isDigit y =
                  first GameBoardError $ B.mkPoint (digitToInt x) (digitToInt y)
readPoint xs = throwError $ InvalidInput xs

declareResult :: AppIO m => Cell -> GameXO m ()
declareResult CellEmpty = gamePutStrLn "It's a draw"
declareResult c         = gamePutStrLn $ "Winner is " <> [cellChar c]

cellEmptyIfBoardFull :: B.Board -> Maybe Cell
cellEmptyIfBoardFull b = if B.boardFull b then Just CellEmpty else Nothing

playAgain :: (AppIO m) => PointGetter m -> PointGetter m -> GameXO m ()
playAgain xg og = do
  gamePutStr "Do you want to play again? [Y/N]: "
  ans <- gameGetLine
  if (toLower <$> ans) `elem` ["y", "yes"]
    then playGame xg og
    else gamePutStrLn "Bye!"

-- |Starts the game and manages it. Collects input from the two users
-- turn-by-turn and checks the state of the game at each board update.
playGame
  :: AppIO m
  => PointGetter m  -- Point getter for X
  -> PointGetter m  -- Point getter for O
  -> GameXO m ()
playGame xg og = printBoard B.emptyBoard >>= playGame' xg og True >> playAgain xg og

playGame'
  :: AppIO m
  => PointGetter m
  -> PointGetter m
  -> Bool
  -> B.Board
  -> GameXO m ()
playGame' xg og xt b = do
  gamePutStrLn $ (if xt then "X's " else "O's ") <> "turn"
  p <- if xt then xg b else og b
  if (B.pointOccupied p b)
    then do
      gamePutStrLn $ "Point " <> show p <> " is already occupied. Try again."
      playGame' xg og xt b
    else do
      b' <- first GameBoardError $ B.updateBoard b . B.Move $ (p, if xt then CellX else CellO)
      _  <- printBoard b'
      maybe
        (playGame' xg og (not xt) b')
        declareResult
        (B.winnerExists b' <|> cellEmptyIfBoardFull b')

playerPointGetter :: AppIO m => B.Board -> GameXO m B.Point
playerPointGetter b = act `catchError` \e -> printError e *> playerPointGetter b
  where act = gamePutStr "Enter point: " *> gameGetLine >>= readPoint

printError :: (AppIO m) => Error -> GameXO m ()
printError (GameBoardError (B.PointOccupied p)) =
  gamePutStrLn $ "Error: Cell " <> show p <> " is already occupied."
printError e = gamePutStrLn $ "Error: " <> show e

computerPointGetter :: Monad m => B.Board -> GameXO m B.Point
computerPointGetter b = do
  g      <- strategyState <$> get
  str    <- asks envStrategy
  (p,g') <- first GameStrategyError $ nextPoint str g b
  modify $ putStrategyState g'
  pure p

getUserEnv :: IO UserEnv
getUserEnv = do
  ans    <- putStr "Do you want to play against the Computer? (Y/N): " *> getLine
  if fmap (toLower) ans `elem` ["y", "yes"]
    then pure $ UserEnv True strategyEasyX
    else pure $ UserEnv False strategyEasyX

launchGame :: IO ()
launchGame = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to tic-tac-toe!"
  ue  <- getUserEnv
  g   <- getStdGen
  let
    env  = mkEnv ue
    getX = playerPointGetter
    getO = if isSinglePlayer env then computerPointGetter else playerPointGetter
  res <- rungame env (mkGameState g) (playGame getX getO)
  either print (\_ -> pure ()) res
