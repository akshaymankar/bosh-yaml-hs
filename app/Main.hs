{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Bosh.Yaml
import qualified Data.Attoparsec.Text       as A
import           Data.Text
import           Options.Applicative        as O
import           Options.Applicative.Simple (simpleOptions, simpleVersion)
import           Options.Applicative.Text
import           Options.Applicative.Types
import           Paths_bosh_yaml_hs         as Meta

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . pack)

parseKeyValue :: A.Parser (Text, Text)
parseKeyValue = do
  key <- A.takeWhile (/= '=')
  _ <- A.anyChar
  value <- A.takeText
  return (key, value)

options :: Parser Options
options = Options
  <$> (many . textOption)
    (long "ops-file"
    <> short 'o'
    <> metavar "PATH"
    <> help "Load manifest operations from a YAML file")
  <*> (fromM . manyM . option (attoReadM parseKeyValue))
    (long "var"
    <> short 'v'
    <> metavar "VAR=VALUE"
    <> help "Load manifest operations from a YAML file")
  <*> (many . textOption)
    (long "vars-file"
    <> short 'l'
    <> metavar "PATH"
    <> help "Load variables from a YAML file")
  <*> argument text (metavar "PATH" <> help "Path to a template that will be interpolated")

main :: IO ()
main = do
  (opts, ()) <- simpleOptions
                  $(simpleVersion Meta.version)
                  "Bosh YAML"
                  "Runs `bosh int` better than `bosh int`"
                  options
                  O.empty
  run opts
