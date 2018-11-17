module Bosh.Yaml where

import Bosh.Interpolation
import Bosh.Operation
import Bosh.Operation.Parsers
import Bosh.Operation.Types
import Control.Monad          ((<=<))
import Data.ByteString        as B (putStr)
import Data.HashMap.Strict    as H
import Data.Text
import Data.Yaml

data BoshErr = YamlErr ParseException
             | OpErr OperationErr
  deriving (Show)

data Options = Options { opsFiles  :: [Text]
                       , vars      :: [(Text, Text)]
                       , varsFiles :: [Text]
                       , path      :: Text
                       }
  deriving Show

wrapParseException :: Either ParseException Value -> Either BoshErr Value
wrapParseException (Left l)  = Left $ YamlErr l
wrapParseException (Right r) = Right r

wrapOpErr :: Either OperationErr Value -> Either BoshErr Value
wrapOpErr (Left l)  = Left $ OpErr l
wrapOpErr (Right r) = Right r

operate :: FilePath -> [Operation] -> HashMap Text Text -> IO (Either BoshErr Value)
operate f ops vars = do
  doc <- wrapParseException <$> decodeFileEither f
  return $ (wrapOpErr . (flip interpolate vars <=< applyOps ops)) =<< doc

readOpsFiles :: [FilePath] -> IO (Either ParseException [Operation])
readOpsFiles files = do
  opss <- mapM decodeFileEither files
  return (Prelude.concat <$> sequence opss)

zipM :: Monad m => m a -> m b -> m (a,b)
zipM ma mb = do
  a <- ma
  b <- mb
  return (a,b)

readAllVars :: Options -> IO (Either ParseException (HashMap Text Text))
readAllVars opts = do
  eitherfileVars <- mapM (decodeFileEither . unpack) $ varsFiles opts
  return $ do
    fileVars <- Prelude.concat <$> sequence eitherfileVars
    return $ H.fromList (vars opts ++ fileVars)

run :: Options -> IO ()
run opts = do
  opsOrErr <- readOpsFiles $ unpack <$> opsFiles opts
  varsOrErr <- readAllVars opts
  case zipM opsOrErr varsOrErr of
    Left e    -> putStrLn $ "Error!!\n" ++ show e
    Right (ops, vars) -> do
      res <- operate (unpack $ path opts) ops vars
      case res of
        Left e    -> putStrLn $ "Error!!\n" ++ show e
        Right val -> B.putStr $ encode val
