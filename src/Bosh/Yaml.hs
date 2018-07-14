module Bosh.Yaml where

import Data.Yaml
import Bosh.Operation

int :: FilePath -> Operation -> IO (Either ParseException Value)
int f o = do
  ev <- decodeFileEither f
  return $ fmap (\v -> applyOperation v o) ev
