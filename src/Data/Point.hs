module Data.Point (Point (Point), unpoint) where

newtype Point = Point
  { unpoint :: (Int, Int)
  } deriving (Eq)

instance Show Point where
  show = show . unpoint
