{-# LANGUAGE OverloadedStrings #-}

module Disk.Analyze where

import Data.List (intercalate)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Text.Lazy as T
import Formatting
import Statistics.Sample
import Statistics.Sample.Histogram

data Statistics = Statistics {
    minimumRate :: !Double,
    maximumRate :: !Double,
    meanRate    :: !Double,
    stdDevRate  :: !Double
} deriving Eq

instance Show Statistics where
    show = reportStatistics

analyzeMeasurements :: Vector Double -> Statistics
analyzeMeasurements observations =
    Statistics
        (V.minimum observations)
        (V.maximum observations)
        (mean observations)
        (stdDev observations)


-- TODO replace with Builder
reportStatistics :: Statistics -> String
reportStatistics s = intercalate "\n" [
    formatToString f "maximum" (maximumRate s),
    formatToString f "minimum" (minimumRate s),
    formatToString f "mean" (meanRate s),
    formatToString f "std dev" (stdDevRate s)]
  where
    f = (right 10 ' ' %. string) % (left 10 ' ' %. fixed 1) % " kB/s"
    

data Histogram = Histogram {
    lowerBoundBucket :: Vector Double,
    countInBucket :: Vector Int
} deriving (Eq, Show)


analyzeToBuckets :: Vector Double -> Histogram
analyzeToBuckets observations =
  let
    (l,c) = histogram 10 observations
  in
    Histogram l c

