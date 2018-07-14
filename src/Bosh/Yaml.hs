module Bosh.Yaml where

import Data.Yaml

int :: FilePath -> IO (Either ParseException Value)
int = decodeFileEither
