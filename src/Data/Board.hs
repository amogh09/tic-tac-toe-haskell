{-# LANGUAGE FlexibleContexts #-}

module Data.Board
  ( Board
  , unboard
  , mkBoard
  , emptyBoard
  , updateBoard
  , winnerExists
  , extractCell
  , boardFull
  , emptyPoints
  , BoardError (..)
  , pointOccupied
  , mkPoint
  , Point
  , Move (Move)
  , unmove
  ) where

import           Control.Applicative  (Alternative, empty, (<|>))
import           Control.Monad.Except (MonadError, throwError)
import           Data.Cell            (Cell (CellEmpty, CellO, CellX), cellChar)
import           Data.List            (intersperse)
import qualified Data.Vector          as V
import qualified Data.VectorUtil      as V

newtype Move = Move
  { unmove :: (Point, Cell)
  } deriving Show

newtype Point = Point
  { unpoint :: (Int, Int)
  } deriving (Eq)

instance Show Point where
  show = show . unpoint

mkPoint :: MonadError BoardError m => Int -> Int -> m Point
mkPoint x y | x >= 0 && x < 3 && y >= 0 && y < 3 = pure $ Point (x,y)
mkPoint x y = throwError $ PointOutOfBoard (x,y)

newtype Board = Board
  { unboard :: V.Vector (V.Vector Cell)
  } deriving (Eq)

data BoardError = InvalidGridSize (Int,Int)
                | PointOccupied Point
                | PointOutOfBoard (Int,Int)
                deriving (Show, Eq)

mkBoard :: MonadError BoardError m => V.Vector (V.Vector Cell) -> m Board
mkBoard grid | V.length grid == 3 && V.length (V.head grid) == 3 = pure $ Board grid
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

pointOccupied :: Point -> Board -> Bool
pointOccupied p b = extractCell p b /= CellEmpty

boardFull :: Board -> Bool
boardFull = not . any id . fmap (V.elem CellEmpty) . unboard

extractCell :: Point -> Board -> Cell
extractCell p b =
  let (x,y) = unpoint p
  in  unboard b V.! x V.! y

updateBoard :: MonadError BoardError m => Board -> Move -> m Board
updateBoard b mv = do
  let
    (p, c') = unmove mv
    (x, y)  = unpoint p
    c       = extractCell p b
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

points :: [Point]
points = [ Point (i,j) | i <- [0..2], j <- [0..2] ]

moves :: Board -> [Move]
moves = fmap Move . zip points . concat . fmap V.toList . V.toList . unboard

emptyPoints :: Board -> [Point]
emptyPoints = fmap fst . filter ((==CellEmpty) . snd) . fmap unmove . moves
