import qualified Data.BoardSpec      as B
import qualified Data.VectorUtilSpec as V
import qualified GameSpec            as G

main :: IO ()
main = do
  B.testAll
  V.testAll
  G.testAll
