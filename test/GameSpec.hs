{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameSpec (testAll) where

import           Control.Monad.State  (MonadState, State, get, modify, runState)
import           Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import           Data.Cell            (Cell (..))
import           Data.Env             (UserEnv (..), mkEnv)
import           Data.State           (mkGameState)
import           Data.Strategy        (strategyEasyX)
import           Game                 (playGame, playerPointGetter, rungame)
import           System.Random        (mkStdGen)
import           Test.Hspec           (describe, hspec, it, shouldBe)
import           Types                (AppIO, gameGetLine, gamePutStr,
                                       gamePutStrLn)

newtype TestIO a = TestIO
  { unTestIO :: WriterT [String] (State [String]) a  }
  deriving (Functor, Applicative, Monad, MonadState [String], MonadWriter [String])

instance AppIO TestIO where
  gamePutStr = tell . (:[])
  gamePutStrLn = gamePutStr
  gameGetLine = (head <$> get) <* modify tail

runTestIO :: [String] -> TestIO a -> ((a,[String]),[String])
runTestIO s = flip runState s . runWriterT . unTestIO

runTest :: [String] -> Cell -> IO ()
runTest ls w = do
  let
    env = mkEnv $ UserEnv False strategyEasyX
    g   = mkStdGen 1
    ppg = playerPointGetter
    res = runTestIO ls $
      either (error . show) fst <$>
      rungame env (mkGameState g) (playGame ppg ppg)
    (r,is) = (head . drop 2 . reverse . snd . fst $ res, snd res)
  is `shouldBe` []
  case w of
    CellX     -> r `shouldBe` "Winner is X"
    CellO     -> r `shouldBe` "Winner is O"
    CellEmpty -> r `shouldBe` "It's a draw"

testAll :: IO ()
testAll = hspec $ do
  describe "game" $ do
    it "declares X as winner - 1" $
      runTest ["00","01","11","02","22","N"] CellX
    it "declares X as winner - 2" $
      runTest
        [ "00","01","11","02","22","Y"
        , "11","22","20","12","02","N"
        ]
        CellX
    it "declares X as winner - 3" $
      runTest ["00","20","02","01","22","11","12","N"] CellX
    it "declares X as winner - 4" $
      runTest ["00","11","22","10","12","02","20","01","21","N"] CellX
    it "declares O as winner - 1" $
      runTest ["01","22","02","00","10","11","N"] CellO
    it "declares game as draw - 1" $
      runTest ["00","11","22","10","12","02","20","21","01","N"] CellEmpty
    it "handles invalid input - 1" $
      runTest ["00","01","yes","no","11","what?","02","22","N"] CellX
    it "handles invalid input - 2" $
      runTest ["123","00","20","02","01","54","22","11","11","11","oh","12","N"] CellX
    it "handles play again" $
      runTest
        [ "00","01","11","02","22","Y"
        , "00","01","11","02","22","yEs"
        , "00","01","11","02","22","YES"
        , "11","22","20","12","02","nehh"
        ]
        CellX

