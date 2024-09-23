{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Opts
  ( Cmd(..)
  , parse
  ) where

import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Options.Applicative
import Prelude hiding (init)
import System.Directory
  ( getCurrentDirectory
  , getXdgDirectory
  , XdgDirectory(..)
  )
import System.FilePath (takeExtension)
import System.IO.Unsafe (unsafePerformIO)

import T qualified
import T.App.Init.Cfg qualified as Init
  ( Cfg(..)
  , InitTmpl(..)
  )


data Cmd
  = Render FilePath T.Scope
  | Init Init.Cfg

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
  path <-
    argument str
      ( metavar "PATH"
     <> help "Template file path"
      )
  env <- envP
  pure (Render path env)

initP :: Parser Cmd
initP = do
  tmpl <-
    argument initTmplR
      ( metavar "PATH"
     <> help "init template name or path"
      )
  env <- envP
  rootDir <-
    option str
      ( long "directory"
     <> short 'c'
     <> metavar "DIR"
     <> help "Use this directory as the base directory for paths in the .ini.t file"
     <> value currentDirectory
      )
  pure $ Init Init.Cfg
    { tmplDir = tmplDirectory
    , ..
    }

envP :: Parser T.Scope
envP =
  option jsonR
    ( long "override"
   <> metavar "JSON"
   <> help "Environment"
   <> value mempty
    )

initTmplR :: ReadM Init.InitTmpl
initTmplR =
  eitherReader (pure . r)
 where
  r val
    | takeExtension val == ".t" = Init.Path val
    | otherwise = Init.Name val

jsonR :: ReadM T.Scope
jsonR =
  eitherReader r
 where
  r =
    fmap (T.Scope . fmap T.reifyAeson) . Aeson.eitherDecode . fromString

currentDirectory :: FilePath
currentDirectory =
  unsafePerformIO getCurrentDirectory
{-# NOINLINE currentDirectory #-}

tmplDirectory :: FilePath
tmplDirectory =
  unsafePerformIO (getXdgDirectory XdgConfig "t/init")
{-# NOINLINE tmplDirectory #-}
