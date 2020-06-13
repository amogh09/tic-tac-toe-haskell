module CommonSpec (it') where

import           Test.Hspec      (SpecWith, it)
import           Test.QuickCheck (Testable, property)

it' :: Testable prop => String -> prop -> SpecWith ()
it' desc p = it desc $ property $ p
