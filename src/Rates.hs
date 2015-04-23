module Main where

import qualified Data.ByteString.Lazy.Char8 as L

import Disk.Input
import Disk.Analyze
import System.Environment

main :: IO ()
main = do
    -- Read stdin, lazily
    args <- getArgs
    input <- if length args > 0
        then L.readFile (args !! 0)
        else L.getContents

    let observations = processIntoVector input

    let result = analyzeMeasurements observations
    print result
        
    let histogram = analyzeToBuckets observations
    print histogram

