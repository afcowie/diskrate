module Main where

import qualified Data.ByteString.Lazy.Char8 as L

import Disk.Input
import Disk.Analyze

main :: IO ()
main = do
    -- Read stdin, lazily
    input <- L.getContents

    -- Grind this down to individual lines. We could have done this in the
    -- parser but this is cleaner, as it means we have a clean slate when a
    -- parse fails.
    let inputs = L.lines input

    -- turn raw lines into Observation objects
    let observations = processIntoVector inputs

    let result = analyzeMeasurements observations

    print result
        

