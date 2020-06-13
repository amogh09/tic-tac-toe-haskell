module Data.VectorUtil (transpose) where

import qualified Data.Vector as V

transpose :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transpose grid | V.null grid = grid
transpose grid = V.fromList [extractCol j grid | j <- [0 .. V.length (grid V.! 0) - 1]]
  where extractCol j = fmap (V.! j)
