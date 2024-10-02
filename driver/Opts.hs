{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Opts
  ( Cmd(..)
  , parse
  ) where

import Data.Aeson qualified as Aeson
import Options.Applicative
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
import T.App.Render.Cfg qualified as Render
  ( Cfg(..)
  , RenderTmpl(..)
  )
import T.Prelude


data Cmd
  = Render Render.Cfg
  | Init Init.Cfg
  | Repl

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
      (info renderP (progDesc "render a template file or string"))
   <> command "init"
      (info initP (progDesc "init a directory"))
   <> command "repl"
      (info replP (progDesc "start REPL"))
    )

renderP :: Parser Cmd
renderP = do
  tmpls <-
    some $
      option (map Render.Path str)
        ( long "path"
       <> short 'f'
       <> metavar "PATH"
       <> help "Template file path"
        )
     <|>
      option (map Render.String str)
        ( long "str"
       <> help "Template string"
        )
  env <- envP
  pure (Render Render.Cfg {..})

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

replP :: Parser Cmd
replP =
  pure Repl

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
    map (T.Scope . map T.embedAeson) . Aeson.eitherDecode . fromString

currentDirectory :: FilePath
currentDirectory =
  unsafePerformIO getCurrentDirectory
{-# NOINLINE currentDirectory #-}

tmplDirectory :: FilePath
tmplDirectory =
  unsafePerformIO (getXdgDirectory XdgConfig "t/init")
{-# NOINLINE tmplDirectory #-}
