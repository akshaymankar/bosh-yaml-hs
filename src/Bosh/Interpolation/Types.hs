module Bosh.Interpolation.Types where

import Bosh.Operation.Types
import Data.Scientific
import Data.HashMap.Strict
import Data.Text
import Data.Yaml

type Variable = Text

type Leaf = (OperationPath, LeafValue)

data LeafValue = StringValue InterpolExpr
               | NumValue Scientific
               | BoolValue Bool
               | NullValue
  deriving (Show, Eq)

data InterpolExpr = IEVar Text InterpolExpr
                  | IEText Text InterpolExpr
                  | End
  deriving (Show, Eq)

data Interpol = Interpol { value :: Value, replacements :: HashMap OperationPath Variable }
