{-# LANGUAGE FlexibleContexts #-}

module Data.Board
  ( Board
  , unboard
  , mkBoard
  , emptyBoard
  , updateBoard
  , winnerExists
  , extractCell
  , pointInBoard
  , boardFull
  ) where

import           Control.Applicative  (Alternative, empty, (<|>))
import           Control.Monad.Except (MonadError, throwError)
import           Data.Cell            (Cell (CellEmpty, CellO, CellX), cellChar)
import           Data.Error           (Error (InvalidGridSize, PointOccupied, PointOutOfBoard))
import           Data.List            (intersperse)
import           Data.Move            (Move, unmove)
import           Data.Point           (Point (..))
import qualified Data.Vector          as V
import qualified Data.VectorUtil      as V

newtype Board = Board
  { unboard :: V.Vector (V.Vector Cell)
  } deriving (Eq)

mkBoard :: MonadError Error m => V.Vector (V.Vector Cell) -> m Board
mkBoard grid | V.length grid == 3 && V.length (V.head grid) ==
               3 = pure $ Board grid
mkBoard grid | V.length grid > 0 =
               throwError . InvalidGridSize $ (V.length grid, V.length (V.head grid))
mkBoard _ = throwError . InvalidGridSize $ (0,0)

emptyBoard :: Board
emptyBoard = Board $ V.replicate 3 $ V.replicate 3 CellEmpty

instance Show Board where
  show =
      unlines
    . V.toList
    . fmap (intersperse ' ' . fmap cellChar . V.toList)
    . unboard

pointInBoard :: Point -> Bool
pointInBoard p =
  let
    (x,y) = unpoint p
  in
    0 <= x && x <= 2 && 0 <= y && y <= 2

boardFull :: Board -> Bool
boardFull = not . any id . fmap (V.elem CellEmpty) . unboard

extractCell :: MonadError Error m => Point -> Board -> m Cell
extractCell p _ | not (pointInBoard p) = throwError $ PointOutOfBoard p
extractCell p b =
  let (x,y) = unpoint p
  in  pure $ unboard b V.! x V.! y

updateBoard :: MonadError Error m => Board -> Move -> m Board
updateBoard b mv = do
  let
    (p, c') = unmove mv
    (x, y)  = unpoint p
  c <- extractCell p b
  if c == CellEmpty
    then pure . Board $ unboard b V.// [(x, unboard b V.! x V.// [(y, c')])]
    else throwError $ PointOccupied p

winnerExists :: Board -> Maybe Cell
winnerExists b =
  horizontalWinner b <|> verticalWinner b <|> fstDiagonalWinner b <|> sndDiagonalWinner b

horizontalWinner :: Board -> Maybe Cell
horizontalWinner = aconcat . V.toList . fmap (winCheck . V.toList) . unboard where

verticalWinner :: Board -> Maybe Cell
verticalWinner = horizontalWinner . Board . V.transpose . unboard

fstDiagonalWinner :: Board -> Maybe Cell
fstDiagonalWinner b = winCheck $ [ grid V.! i V.! i | i <- [0 .. V.length grid - 1] ]
  where grid = unboard b

sndDiagonalWinner :: Board -> Maybe Cell
sndDiagonalWinner b = winCheck $ [ grid V.! i V.! (n-i-1) | i <- [0 .. V.length grid - 1] ]
  where
    grid = unboard b
    n = V.length grid

aconcat :: Alternative f => [f a] -> f a
aconcat []     = empty
aconcat (x:xs) = x <|> aconcat xs

winCheck :: [Cell] -> Maybe Cell
winCheck line | all (uncurry (==)) $ zip line (repeat CellX) = Just CellX
winCheck line | all (uncurry (==)) $ zip line (repeat CellO) = Just CellO
winCheck _    = Nothing
