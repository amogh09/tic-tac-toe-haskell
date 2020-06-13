{-# LANGUAGE TupleSections #-}

module Data.BoardSpec (testAll) where

import           CommonSpec          (it')
import           Control.Monad       (foldM)
import           Data.Board          (Board, boardFull, emptyBoard, extractCell,
                                      mkBoard, unboard, updateBoard,
                                      winnerExists)
import           Data.Cell           (Cell (CellEmpty, CellO, CellX))
import           Data.Either         (isRight)
import           Data.Error          (Error (InvalidGridSize, PointOccupied, PointOutOfBoard))
import           Data.List           (nub)
import           Data.Move           (Move (Move), unmove)
import           Data.PointSpec      (pointInBoard, pointOutOfBoard)
import qualified Data.Vector         as V
import qualified Data.VectorUtil     as V
import qualified Data.VectorUtilSpec as V
import           Test.Hspec
import           Test.QuickCheck     (Gen, Property, arbitraryBoundedEnum,
                                      choose, elements, forAll, vectorOf, (==>))

prop_emptyBoard_row_size :: Bool
prop_emptyBoard_row_size = (==3) . V.length . unboard $ emptyBoard

prop_emptyBoard_col_size :: Bool
prop_emptyBoard_col_size = (==3) . V.length . V.head . unboard $ emptyBoard

prop_emptyBoard_all_empty :: Bool
prop_emptyBoard_all_empty =
  all (==CellEmpty) . concat . fmap V.toList . V.toList . unboard $ emptyBoard

cellArbitrary :: Gen Cell
cellArbitrary = arbitraryBoundedEnum

cellXO :: Gen Cell
cellXO = elements [CellX, CellO]

boardArbitrary :: Gen Cell -> Gen Board
boardArbitrary c = mkBoard' <$> V.gridArbitrary (3,3) c

prop_updateBoard_out_of_board_move :: Property
prop_updateBoard_out_of_board_move =
  forAll (boardArbitrary cellArbitrary)  $ \b ->
  forAll cellArbitrary   $ \c ->
  forAll pointOutOfBoard $ \p ->
  (updateBoard b (Move (p,c)) :: Either Error Board) == Left (PointOutOfBoard p)

prop_updateBoard_point_occupied_err :: Property
prop_updateBoard_point_occupied_err =
  forAll pointInBoard $ \p ->
  forAll cellXO       $ \c ->
  let
    m = Move (p,c)
    b = updateBoard emptyBoard m
  in
    either (const False) ((== Left (PointOccupied p)) . flip updateBoard m) b

prop_updateBoard_valid_move :: Property
prop_updateBoard_valid_move =
  forAll pointInBoard $ \p ->
  forAll cellXO       $ \c ->
  let
    m = Move (p,c)
  in
    (updateBoard emptyBoard m >>= extractCell p) == Right c

genHorizontalWinBoard :: Cell -> Gen Board
genHorizontalWinBoard c =
  let
    emptyGrid = V.replicate 3 (V.replicate 3 CellEmpty)
  in
        mkBoard'
    <$> (emptyGrid V.//) . (:[])
    <$> ((,V.replicate 3 c) <$> choose (0,2))

genVerticalWinBoard :: Cell -> Gen Board
genVerticalWinBoard c = (mkBoard' . V.transpose . unboard) <$> genHorizontalWinBoard c

prop_horizontal_win :: Property
prop_horizontal_win =
  forAll cellXO $ \c ->
  forAll (genHorizontalWinBoard c) $ (==Just c) . winnerExists

prop_vertical_win :: Property
prop_vertical_win =
  forAll cellXO $ \c ->
  forAll (genVerticalWinBoard c) $ (==Just c) . winnerExists

fstDiagonalWinBoard :: Cell -> Gen Board
fstDiagonalWinBoard c = mkBoard' <$> V.fromList <$> sequence
  [ V.fromList <$> sequence [pure c, cellArbitrary, cellArbitrary]
  , V.fromList <$> sequence [cellArbitrary, pure c, cellArbitrary]
  , V.fromList <$> sequence [cellArbitrary, cellArbitrary, pure c]
  ]

mkBoard' :: V.Vector (V.Vector Cell) -> Board
mkBoard' = either (error . show) id . mkBoard

sndDiagonalWinBoard :: Cell -> Gen Board
sndDiagonalWinBoard c = mkBoard' <$> V.fromList <$> sequence
  [ V.fromList <$> sequence [cellArbitrary, cellArbitrary, pure c]
  , V.fromList <$> sequence [cellArbitrary, pure c, cellArbitrary]
  , V.fromList <$> sequence [pure c, cellArbitrary, cellArbitrary]
  ]

prop_fst_diagonal_win :: Property
prop_fst_diagonal_win =
  forAll cellXO $ \c ->
  forAll (fstDiagonalWinBoard c) $ (==Just c) . winnerExists

prop_snd_diagonal_win :: Property
prop_snd_diagonal_win =
  forAll cellXO $ \c ->
  forAll (sndDiagonalWinBoard c) $ (==Just c) . winnerExists

prop_winnerExists_emptyBoard :: Bool
prop_winnerExists_emptyBoard = winnerExists emptyBoard == Nothing

prop_board_eq :: Property
prop_board_eq = forAll (boardArbitrary cellArbitrary) $ \b -> b == b

prop_board_not_eq_diff_points :: Property
prop_board_not_eq_diff_points =
  forAll pointInBoard $ \p  ->
  forAll pointInBoard $ \p' ->
  p /= p' ==>
  forAll cellXO       $ \c  ->
  let
    b1 = updateBoard emptyBoard (Move (p,c))
    b2 = updateBoard emptyBoard (Move (p',c))
  in
    isRight b1 && isRight b2 && b1 /= b2

prop_board_not_eq_diff_cells :: Property
prop_board_not_eq_diff_cells =
  forAll cellXO       $ \c  ->
  forAll cellXO       $ \c' ->
  c /= c' ==>
  forAll pointInBoard $ \p  ->
  let
    b1 = updateBoard emptyBoard (Move (p,c))
    b2 = updateBoard emptyBoard (Move (p,c'))
  in
    isRight b1 && isRight b2 && b1 /= b2

prop_mkBoard_wrong_grid_size :: Property
prop_mkBoard_wrong_grid_size =
  forAll (choose (0,20)) $ \r ->
  forAll (choose (0,20)) $ \c ->
  r /= 3 && c /= 3 ==>
  forAll (V.gridArbitrary (r,c) cellArbitrary) $ \g ->
  if r == 0
    then mkBoard g == Left (InvalidGridSize (0,0))
    else mkBoard g == Left (InvalidGridSize (r,c))

prop_boardFull :: Property
prop_boardFull = forAll (boardArbitrary cellXO) $ \b -> boardFull b

nonFullBoardGen :: Gen Board
nonFullBoardGen = do
  p   <- pointInBoard
  i   <- choose (0,20)
  ps  <- nub <$> vectorOf i pointInBoard
  mvs <- fmap Move <$> zip ps <$> vectorOf i cellXO
  pure $ either (error . show) id $
    foldM updateBoard emptyBoard $ filter ((/=p) . fst . unmove) mvs

prop_boardFull_not :: Property
prop_boardFull_not = forAll nonFullBoardGen $ \b -> not $ boardFull b

testAll :: IO ()
testAll = hspec $ do
  describe "emptyBoard" $ do
    it' "has a row size of 3" prop_emptyBoard_row_size
    it' "has a col size of 3" prop_emptyBoard_col_size
    it' "has all empty cells" prop_emptyBoard_all_empty
  describe "updateBoard" $ do
    it' "throws error if move is out of board" prop_updateBoard_out_of_board_move
    it' "throws error if move point occupied" prop_updateBoard_point_occupied_err
    it' "adds cell to an unoccupied point" prop_updateBoard_valid_move
  describe "winnerExists" $ do
    it' "finds horizontal winners" prop_horizontal_win
    it' "finds vertical winners" prop_vertical_win
    it' "finds fst diagonal winners" prop_fst_diagonal_win
    it' "finds snd diagonal winners" prop_snd_diagonal_win
    it' "finds on winner on empty board" prop_winnerExists_emptyBoard
  describe "Board" $ do
    it' "is equal to itself" prop_board_eq
    it'
      "is not equal to a board with same cell at a diff point"
      prop_board_not_eq_diff_points
    it'
      "is not equal to a board with diff cell at same point"
      prop_board_not_eq_diff_cells
  describe "mkBoard" $ do
    it' "fails when row size is invalid" prop_mkBoard_wrong_grid_size
  describe "boardFull" $ do
    it' "detects full boards" prop_boardFull
    it' "returns false for non-full boards" prop_boardFull_not
