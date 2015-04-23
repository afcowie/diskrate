module Disk.Input where

import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Attoparsec.ByteString.Char8 hiding (parse, maybeResult)
import Data.Attoparsec.ByteString.Lazy (parse, maybeResult)
import Data.List (foldl')
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

process :: FilePath -> IO (Vector Double)
process path = do
    input <- L.readFile path
    return $ processIntoVector input

processIntoVector :: ByteString -> Vector Double
processIntoVector input =
    V.fromList $ foldl' processLine [] inputs
  where
    processLine os line =
      let
        result = parse parseInputLine line
      in
        case maybeResult result of
            Just o  -> o:os
            Nothing -> os

    -- Grind this down to individual lines. We could have done this in the
    -- parser but this is cleaner, as it means we have a clean slate when a
    -- parse fails.
    inputs = L.lines input

parseInputLine :: Parser Double
parseInputLine = do
    number  <- many (char ' ') *> double <* many (char ' ')
    return number
