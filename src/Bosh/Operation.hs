module Bosh.Operation where

import Data.Text
import Data.Yaml
import Data.HashMap.Strict

data Operation = Operation { opType :: OperationType, opPath :: OperationPath}
data OperationType = Remove
                   | Replace { replacement :: Value }

data OperationPath = OperationPath Text OperationPath
                   | EndOfPath

applyOp :: Value -> Operation -> Value
applyOp v (Operation t path) =
  case t of
    Remove -> removeOp v path
    (Replace r) -> replaceOp v path r

applyOps :: [Operation] -> Value -> Value
applyOps os v = Prelude.foldl applyOp v os

removeOp :: Value -> OperationPath -> Value
removeOp v path = replaceOp v path Null

replaceOp :: Value -> OperationPath -> Value -> Value
replaceOp v path r =
  case path of
    (OperationPath key rem) ->
      case v of
        (Object x) -> case replaceOp (x ! key) rem r of
                        Null   -> Object $ delete key x
                        newVal -> Object $ insert key newVal x
        _ -> error "foo"
    _ -> r

