{-# LANGUAGE TupleSections #-}
module Bosh.Operation where

import Bosh.Operation.Types
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
replaceOp' (Object o) r path@(MapSegment key _:rem) =
  if member key o
     then replaceOpKeyPresent o r key rem
     else replaceOpKeyAbsent o r path
-- Addition to array operations
replaceOp' (Array a) r [ArraySegment LastIndex]       = return $ Array $ V.snoc a r
replaceOp' (Array a) r [ArraySegment (BeforeIndex i)] = return $ Array $ insertAt i r a
-- Replacement in array operations
replaceOp' (Array a) r (ArraySegment (NumIndex i):rem)     = replaceInArray a r rem $ (i,) <$> (V.!?) a i
replaceOp' (Array a) r (ArraySegment (MapMatcher k v):rem) = replaceInArray a r rem $ lookupForObject a k v
-- Error cases like
-- - arrays with map segment
-- - objects with array segment
-- - extra segments after an addition only segment
-- TODO: Descriptive errors
replaceOp' _ _ (_:_) = Left OperationErr

replaceInArray :: V.Vector Value -> Value -> [PathSegment]
               -> Maybe (Int, Value) -- Maybe (ReplaceHere, ReplaceInThisValue)
               -> Either OperationErr Value
replaceInArray _ _ _ Nothing = Left OperationErr
replaceInArray a r rem (Just (i, f)) = do
      x <- replaceOp' f r rem
      case x of
        Null -> Array <$> deleteNth i a
        _ -> return $ Array $ V.update a (V.fromList [(i, x)])

lookupForObject :: V.Vector Value -> Text -> Text -> Maybe (Int, Value)
lookupForObject a k v = let matchingIndices = V.findIndices (hasKeyValuePair k v) a
                         in if V.length matchingIndices == 1
                               then Just (V.head matchingIndices, (V.!) a $ V.head matchingIndices)
                               else Nothing

hasKeyValuePair :: Text -> Text -> Value -> Bool
hasKeyValuePair key value (Object x) = case Map.lookup key x of
                                Nothing -> False
                                Just (String s) -> s == value
                                Just _ -> False
hasKeyValuePair _ _ _ = False

deleteNth :: Int -> V.Vector a -> Either OperationErr (V.Vector a)
deleteNth n xs = if V.length xs >= n
                    then let (ys,zs) = V.splitAt n xs
                          in Right (V.concat [ys, V.tail zs])
                    else Left OperationErr

insertAt :: Int -> a -> V.Vector a -> V.Vector a
insertAt n x xs = let (ys, zs) = V.splitAt n xs in V.concat [ys, V.fromList [x], zs]

replaceOpKeyPresent :: HashMap Text Value -> Value -> Text -> [PathSegment] -> Either OperationErr Value
replaceOpKeyPresent o r key rem = do
  newTree <- replaceOp' (o ! key) r rem
  case  newTree of
    Null   -> return $ Object $ delete key o
    newVal -> return $ Object $ insert key newVal o

replaceOpKeyAbsent :: HashMap Text Value -> Value -> [PathSegment] -> Either OperationErr Value
replaceOpKeyAbsent _ _ [] = Left OperationErr
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
