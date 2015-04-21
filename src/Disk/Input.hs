module Disk.Input where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.ByteString.Char8 hiding (parse, maybeResult)
import Data.Attoparsec.ByteString.Lazy (parse, maybeResult)
import Data.List (foldl')
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

processIntoVector :: [L.ByteString] -> Vector Double
processIntoVector inputs =
    V.fromList $ foldl' processLine [] inputs
  where
    processLine os line =
      let
        result = parse parseInputLine line
      in
        case maybeResult result of
            Just o  -> o:os
            Nothing -> os

parseInputLine :: Parser Double
parseInputLine = do
    number  <- many (char ' ') *> double <* many (char ' ')
    return number
