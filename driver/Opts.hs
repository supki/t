{-# LANGUAGE ApplicativeDo #-}
module Opts
  ( parse
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.String (fromString)
import           Options.Applicative

import qualified T


parse :: IO (FilePath, T.Env)
parse =
  execParser
    (info
      (parser <**> helper)
      (fullDesc <> progDesc "Templating" <> header "t - templating utility"))

parser :: Parser (FilePath, T.Env)
parser = do
  path <- argument str (metavar "PATH" <> help "Template file path")
  env <- option envR (long "env" <> metavar "JSON" <> help "Environment")
  pure (path, env)

envR :: ReadM T.Env
envR =
  eitherReader (fmap (T.mkDefEnv . HashMap.mapKeys fromString) . Aeson.eitherDecode . fromString)
