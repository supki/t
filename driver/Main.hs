module Main (main) where

import T.App.Init qualified
import T.App.Render qualified
import T.Prelude

import qualified Opts


main :: IO ()
main = do
  cmd <- Opts.parse
  case cmd of
    Opts.Render path env ->
      T.App.Render.run path env
    Opts.Init cfg ->
      T.App.Init.run cfg
