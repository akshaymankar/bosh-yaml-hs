{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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
replaceOp' (Object o) r path@(seg@(MapSegment key optional):rem) =
  if member key o
     then replaceOpKeyPresent o r key rem
     else replaceOpKeyAbsent o r key optional rem
-- Addition to array operations
replaceOp' (Array a) r [ArraySegment LastIndex]       = return $ Array $ V.snoc a r
replaceOp' (Array a) r [ArraySegment (BeforeIndex i)] = return $ Array $ insertAt i r a
replaceOp' (Array a) r (ArraySegment seg@(BeforeIndex i):rem) = Left $ ExtraSegments seg rem
replaceOp' (Array a) r (ArraySegment seg@LastIndex:rem)       = Left $ ExtraSegments seg rem
-- Replacement in array operations
replaceOp' (Array a) r (ArraySegment (NumIndex i):rem)  =
  replaceInArray a r rem (IndexOutOfBounds i (Array a)) $ (i,) <$> (V.!?) a i
replaceOp' (Array a) r (ArraySegment (MapMatcher k v optional):rem) =
  let x = lookupForObject a k v
   in case x of
        Nothing -> if optional
                      then if Prelude.null rem
                              then return $ Array $ V.snoc a r
                              else Left $ ExtraSegments (MapMatcher k v optional) rem
                      else Left $ MapMatcherError k v (Array a)
        j -> replaceInArray a r rem (MapMatcherError k v (Array a)) j
-- TypeMismatch
replaceOp' o _ (seg:_) = Left $ TypeMismatch seg o

replaceInArray :: V.Vector Value -> Value -> [PathSegment]
               -> OperationErr
               -> Maybe (Int, Value) -- Maybe (ReplaceHere, ReplaceInThisValue)
               -> Either OperationErr Value
replaceInArray _ _ _ err Nothing = Left err
replaceInArray a r rem _ (Just (i, f)) = do
      x <- replaceOp' f r rem
      case x of
        Null -> return $ Array $ deleteNth i a
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

deleteNth :: Int -> V.Vector a -> V.Vector a
deleteNth n xs = let (ys,zs) = V.splitAt n xs
                  in V.concat [ys, V.tail zs]

insertAt :: Int -> a -> V.Vector a -> V.Vector a
insertAt n x xs = let (ys, zs) = V.splitAt n xs in V.concat [ys, V.fromList [x], zs]

replaceOpKeyPresent :: HashMap Text Value -> Value -> Text -> [PathSegment] -> Either OperationErr Value
replaceOpKeyPresent o r key rem = do
  newTree <- replaceOp' (o ! key) r rem
  case  newTree of
    Null   -> return $ Object $ delete key o
    newVal -> return $ Object $ insert key newVal o

replaceOpKeyAbsent :: HashMap Text Value -> Value -> Text -> Bool -> [PathSegment] -> Either OperationErr Value
replaceOpKeyAbsent o r seg isOptional rem =
  if isOptional
    then
      if r == Null
         then return (Object o)
         else do
           x <- createObject r rem
           return $ Object $ insert seg x o
    else Left $ MandatoryKeyNotFound seg (Object o)

createObject :: Value -> [PathSegment] -> Either OperationErr Value
createObject v (MapSegment key _:ps) = Object . Map.singleton key <$> createObject v ps
createObject v [] = return v
createObject v [ArraySegment LastIndex] = return $ array [ v ]
createObject _ (seg:_) = Left $ UnexpectedSegment seg
