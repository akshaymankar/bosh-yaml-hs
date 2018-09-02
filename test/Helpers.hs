module Helpers where

import Bosh.Operation.Parsers
import Bosh.Operation.Types
import Data.Attoparsec.Text
import Data.Text as T

readQuery :: Text -> [PathSegment]
readQuery t = case parseOnly (pathParser <* endOfInput) t of
                Right (OperationPath ps) -> ps
                Left e -> error ("Parser failed to parse: " ++ T.unpack t ++ " Error: " ++ e)

