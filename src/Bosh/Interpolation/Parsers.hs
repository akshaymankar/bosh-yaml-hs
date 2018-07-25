{-# LANGUAGE OverloadedStrings #-}
module Bosh.Interpolation.Parsers where

import Bosh.Interpolation.Types
import Control.Applicative ((<|>))
import Data.Attoparsec.Text as AT
import Data.Text

normalizeInterpolExpr :: InterpolExpr -> InterpolExpr
normalizeInterpolExpr (IEText t1 (IEText t2 r)) = IEText (t1 `append` t2) $ normalizeInterpolExpr r
normalizeInterpolExpr (IEText t r) = IEText t $ normalizeInterpolExpr r
normalizeInterpolExpr (IEVar v r) = IEVar v $ normalizeInterpolExpr r
normalizeInterpolExpr End = End

interpolExprParser :: Parser InterpolExpr
interpolExprParser = do
  x <- varParser <|> textOrEndParser
  return $ normalizeInterpolExpr x

textOrEndParser :: Parser InterpolExpr
textOrEndParser = do
  mc <- peekChar
  case mc of
    Nothing -> return End
    -- If there is a '(' in the begining, it clearly means varParser failed to parse it
    -- So we have to assume it is a solitary/extra paren
    -- So, this parser really depends on other parser having run before it. Not very pure I thnk!
    -- TODO: Make this pure
    Just '(' -> IEText <$> string "(" <*> interpolExprParser
    _ -> do
      x <- AT.takeWhile (/= '(')
      if x == ""
         then return End
         else IEText x <$> interpolExprParser

varParser :: Parser InterpolExpr
varParser = do
  _ <- string "(("
  var <- pack <$> many1 (satisfy $ notInClass "()")
  _ <- string "))"
  IEVar var <$> interpolExprParser

parseLeafText :: Text -> LeafValue
parseLeafText t = case parseOnly (interpolExprParser <* endOfInput) t of
  Right e -> StringValue e
  _ -> StringValue $ IEText t End


