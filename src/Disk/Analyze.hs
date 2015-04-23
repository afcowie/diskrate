{-# LANGUAGE OverloadedStrings #-}

module Disk.Analyze where

import Data.List (intercalate)
import Data.Monoid
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import Formatting
import Statistics.Sample
import Statistics.Sample.Histogram
import Statistics.Quantile

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

reportHistogram :: Vector Double -> Text
reportHistogram observations = 
    toLazyText result
  where
    -- calculate the histogram
    (lowerBounds,countsPerBucket) = histogram 10 observations
    -- zip the baases and counts into a Vector of pairs
    buckets = V.zip lowerBounds countsPerBucket
    
    -- accumulate Builders
    result :: Builder
    result = V.foldl f (singleton '\n') buckets

    -- format a bucket
    f :: Builder -> (Double, Int) -> Builder
    f accumulator (l,c) = mappend accumulator (make l c)

    make l c = bprint ((left 10 ' ' %. fixed 2) % (left 7 ' ' %. int) % "\n") l' c
      where
        l' = if l < 0 then 0 else l



reportPercentile :: Int -> Vector Double -> Text
reportPercentile which observations = 
    format f which value
  where
    f = (left 3 ' ' %. int) % (left 10 ' ' %. fixed 1) % " kB/s"
    value = continuousBy spss which 100 observations 
