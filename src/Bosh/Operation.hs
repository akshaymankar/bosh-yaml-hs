module Bosh.Operation where

import Data.Text
import Data.Yaml
import Data.HashMap.Strict

data Operation = Operation { opType :: OperationType, opPath :: OperationPath}
data OperationType = Remove
                   | Replace { replacement :: Value }

data OperationPath = OperationPath Text OperationPath
                   | EndOfPath

applyOperation :: Value -> Operation -> Value
applyOperation v (Operation t path) = case t of
                                        Remove -> remove v path
                                        (Replace _) -> v

remove :: Value -> OperationPath -> Value
remove v path = case path of
                  (OperationPath key rem) ->
                    case v of
                      (Object x) -> case remove (x ! key) rem of
                                      Null   -> Object $ delete key x
                                      newVal -> Object $ insert key newVal x
                      _ -> error "foo"
                  _ -> Null
