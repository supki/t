module Main (main) where

import T.App.Init qualified
import T.App.Render qualified
import T.App.Repl qualified
import T.Prelude

import qualified Opts


main :: IO ()
main = do
  cmd <- Opts.parse
  case cmd of
    Opts.Render cfg ->
      T.App.Render.run cfg
    Opts.Init cfg ->
      T.App.Init.run cfg
    Opts.Repl ->
      T.App.Repl.run
