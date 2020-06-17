module GameSpec (testAll) where

import           Control.Exception       (finally)
import           Data.Cell               (Cell (CellEmpty, CellO, CellX),
                                          cellChar)
import           Data.Env                (UserEnv (..), mkEnv, mkIOEnv)
import           Data.State              (mkGameState)
import           Data.Strategy           (strategyEasyX)
import           Game                    (playGame, playerPointGetter, rungame)
import           System.Directory        (removeFile)
import           System.IO               (SeekMode (AbsoluteSeek), hGetContents,
                                          hPutStr, hSeek, openTempFile)
import           System.Random           (getStdGen)
import           Test.Hspec              (Expectation, describe, hspec, it)
import           Test.Hspec.Expectations (shouldReturn)

runTest :: [String] -> IO [String]
runTest ls = do
  (rfp,rh) <- openTempFile "/tmp" "tic-tac-toe.in"
  (wfp,wh) <- openTempFile "/tmp" "tic-tac-toe.out"
  go playerPointGetter rh wh `finally` (removeFile rfp *> removeFile wfp)
  where
    go ppg rh wh = do
      hPutStr rh $ unlines ls
      hSeek rh AbsoluteSeek 0
      let env = mkEnv (mkIOEnv rh wh) (UserEnv False strategyEasyX)
      g <- getStdGen
      _ <- either (error . show) pure =<< rungame env (mkGameState g) (playGame ppg ppg)
      hSeek wh AbsoluteSeek 0
      lines <$> hGetContents wh

gameTest :: [String] -> Cell -> Expectation
gameTest mvs c =
  let
    st = case c of
      CellEmpty -> "It's a draw"
      _         -> "Winner is " <> [cellChar c]
  in
    (head . tail . reverse) <$> (runTest mvs)
    `shouldReturn` st

testAll :: IO ()
testAll = hspec $ do
  describe "game" $ do
    it "declares X as winner - 1" $
      gameTest ["00","01","11","02","22","N"] CellX
    it "declares X as winner - 2" $
      gameTest
        [ "00","01","11","02","22","Y"
        , "11","22","20","12","02","N"
        ]
        CellX
    it "declares X as winner - 3" $
      gameTest ["00","20","02","01","22","11","12","N"] CellX
    it "declares X as winner - 4" $
      gameTest ["00","11","22","10","12","02","20","01","21","N"] CellX
    it "declares O as winner - 1" $
      gameTest ["01","22","02","00","10","11","N"] CellO
    it "declares game as draw - 1" $
      gameTest ["00","11","22","10","12","02","20","21","01","N"] CellEmpty
    it "handles invalid input - 1" $
      gameTest ["00","01","yes","no","11","what?","02","22","N"] CellX
    it "handles invalid input - 2" $
      gameTest ["123","00","20","02","01","54","22","11","11","11","oh","12","N"] CellX
    it "handles play again" $
      gameTest
        [ "00","01","11","02","22","Y"
        , "00","01","11","02","22","yEs"
        , "00","01","11","02","22","YES"
        , "11","22","20","12","02","nehh"
        ]
        CellX

