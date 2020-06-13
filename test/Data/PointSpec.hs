module Data.PointSpec (pointInBoard, pointOutOfBoard, pointArbitrary) where

import           Data.Point      (Point (Point))
import           Test.QuickCheck (Gen, choose, elements, frequency, oneof)

intForPoint :: Gen Int
intForPoint = frequency
    [ (1, elements [(-5)..(-1)])
    , (12, elements [0..2])
    , (1,elements [3..5])
    ]

pointInBoard :: Gen Point
pointInBoard = Point <$> ((,) <$> choose (0,2) <*> choose (0,2))

outOfBoardPosInt :: Gen Int
outOfBoardPosInt = choose (3,10)

outOfBoardNegInt :: Gen Int
outOfBoardNegInt = choose (-10,-1)

outOfBoardInt :: Gen Int
outOfBoardInt = oneof [outOfBoardNegInt, outOfBoardPosInt]

pointOutOfBoard :: Gen Point
pointOutOfBoard = Point <$> ((,) <$> outOfBoardInt <*> outOfBoardInt)

pointArbitrary :: Gen Point
pointArbitrary = Point <$> ((,) <$> intForPoint <*> intForPoint)
