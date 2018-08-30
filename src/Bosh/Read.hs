module Bosh.Read where

import Bosh.Operation
import Bosh.Operation.Types
import Data.HashMap.Strict as M
import Data.Vector as V
import Data.Yaml

readYaml :: [PathSegment] ->  Value -> Maybe Value
readYaml [] v = Just v
readYaml (p:ps) v = readYaml ps =<< yamlLookup p v

yamlLookup :: PathSegment -> Value -> Maybe Value
yamlLookup (MapSegment p _)                  (Object o) = M.lookup p o
yamlLookup (ArraySegment (NumIndex n))       (Array a)  = a !? n
yamlLookup (ArraySegment (MapMatcher k v _)) (Array a)  = snd <$> lookupForObject a k v
yamlLookup _ _ = undefined
