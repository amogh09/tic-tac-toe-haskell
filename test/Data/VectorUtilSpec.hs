module Data.VectorUtilSpec (testAll, gridArbitrary) where

import           CommonSpec      (it')
import           Data.List       (transpose)
import qualified Data.Vector     as V
import qualified Data.VectorUtil as V
import           Test.Hspec      (describe, hspec)
import           Test.QuickCheck (Gen, Property, choose, forAll, vectorOf)

gridArbitrary :: (Int,Int) -> Gen a -> Gen (V.Vector (V.Vector a))
gridArbitrary (r,c) g =
  V.fromList <$> vectorOf r (V.fromList <$> vectorOf c g)

gridToList :: V.Vector (V.Vector c) -> [[c]]
gridToList = V.toList . fmap V.toList

gridFromList :: [[c]] -> V.Vector (V.Vector c)
gridFromList = V.fromList . fmap V.fromList

prop_transpose :: Property
prop_transpose =
  forAll (choose (0,10)) $ \r ->
  forAll (choose (0,10)) $ \c ->
  forAll (gridArbitrary (r,c) (choose (-10,10 :: Int))) $ \g ->
  V.transpose g == (gridFromList . transpose . gridToList $ g)

testAll :: IO ()
testAll = hspec $ do
  describe "transpose" $ do
    it' "transposes correctly" prop_transpose
