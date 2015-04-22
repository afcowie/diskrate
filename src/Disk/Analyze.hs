module Disk.Analyze where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Statistics.Sample

data Statistics = Statistics {
    minimumRate :: !Double,
    maximumRate :: !Double,
    meanRate    :: !Double,
    stdDevRate  :: !Double
} deriving (Eq,Show)

{-
instance Show Statistics where
    show = reportStatistics
-}

analyzeMeasurements :: Vector Double -> Statistics
analyzeMeasurements observations =
    Statistics
        (V.minimum observations)
        (V.maximum observations)
        (mean observations)
        (stdDev observations)



