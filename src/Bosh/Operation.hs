module Bosh.Operation where

import Bosh.Types
import Control.Monad
import Data.Aeson.Types
import Data.Functor
import Data.HashMap.Strict as Map
import Data.Text
import Data.Yaml

import qualified Data.Attoparsec.Text as AT
import qualified Data.Vector as V

applyOp :: Value -> Operation -> Either OperationErr Value
applyOp v (Operation t path) =
  case t of
    Remove -> removeOp v path
    (Replace r) -> replaceOp v path r

applyOps :: [Operation] -> Value -> Either OperationErr Value
applyOps os v = foldM applyOp v os

removeOp :: Value -> OperationPath -> Either OperationErr Value
removeOp v path = replaceOp v path Null

replaceOp :: Value -> OperationPath -> Value -> Either OperationErr Value
replaceOp doc (OperationPath path) r = replaceOp' doc r path

replaceOp' :: Value -> Value -> [PathSegment] -> Either OperationErr Value
replaceOp' _ r [] = return r
replaceOp' (Object o) r path@(seg@(MapSegment key _):rem) =
  if member key o
     then replaceOpKeyPresent o r key rem
     else replaceOpKeyAbsent o r path

replaceOp' (Array a) r    path@(seg@(ArraySegment LastIndex):rem)    = return $ Array $ V.snoc a r
replaceOp' (Array a) Null path@(seg@(ArraySegment (NumIndex i)):rem) = return $ Array $ deleteNth i a
replaceOp' (Array a) r    path@(seg@(ArraySegment (NumIndex i)):rem) = return $ Array $ V.update a (V.fromList [(i, r)])

replaceOp' (Array a) r    path@(seg@(ArraySegment (BeforeIndex i)):rem) = return $ Array $ insertAt i r a

replaceOp' _ _ (_:_) = Left OperationErr

deleteNth :: Int -> V.Vector a -> V.Vector a
deleteNth n xs = let (ys,zs) = V.splitAt n xs in V.concat [ys, V.tail zs]

insertAt :: Int -> a -> V.Vector a -> V.Vector a
insertAt n x xs = let (ys, zs) = V.splitAt n xs in V.concat [ys, V.fromList [x], zs]

replaceOpKeyPresent o r key rem = do
  newTree <- replaceOp' (o ! key) r rem
  case  newTree of
    Null   -> return $ Object $ delete key o
    newVal -> return $ Object $ insert key newVal o

replaceOpKeyAbsent :: HashMap Text Value -> Value -> [PathSegment] -> Either OperationErr Value
replaceOpKeyAbsent o r path@(seg:rem) =
  if isOptional seg
    then
      if r == Null
         then return (Object o)
         else return $ createObject r path
    else Left OperationErr

createObject :: Value -> [PathSegment] -> Value
createObject v (p:ps) = Object $ Map.singleton (segment p) $ createObject v ps
createObject v [] = v
