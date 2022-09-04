{-# LANGUAGE ApplicativeDo #-}
module Opts
  ( parse
  ) where

import Options.Applicative


parse :: IO (FilePath, String)
parse =
  execParser
    (info
      (parser <**> helper)
      (fullDesc <> progDesc "Templating" <> header "t - templating utility"))

parser :: Parser (FilePath, String)
parser = do
  path <- argument str (metavar "PATH" <> help "Template file path")
  env <- strOption (long "env" <> metavar "JSON" <> help "Environment")
  pure (path, env)
