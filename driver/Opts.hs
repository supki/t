{-# LANGUAGE ApplicativeDo #-}
module Opts
  ( Cmd(..)
  , parse
  ) where

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.String (fromString)
import Options.Applicative

import T qualified


data Cmd
  = Render FilePath T.Env
  | Init FilePath T.Env

parse :: IO Cmd
parse =
  execParser
    (info
      (parser <**> helper)
      (fullDesc <> progDesc "Templating" <> header "t - templating utility"))

parser :: Parser Cmd
parser =
  subparser
    ( command "render"
      (info renderP (progDesc "render a template"))
   <> command "init"
      (info initP (progDesc "init a directory"))
    )

renderP :: Parser Cmd
renderP = do
  path <- argument str (metavar "PATH" <> help "Template file path")
  env <- option envR (long "env" <> metavar "JSON" <> help "Environment")
  pure (Render path env)

initP :: Parser Cmd
initP = do
  path <- argument str (metavar "PATH" <> help "Template file path")
  env <- option envR (long "env" <> metavar "JSON" <> help "Environment")
  pure (Init path env)

envR :: ReadM T.Env
envR =
  eitherReader (fmap (T.mkDefEnv . fmap T.reifyAeson . HashMap.mapKeys fromString) . Aeson.eitherDecode . fromString)
