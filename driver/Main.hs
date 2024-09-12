{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Prelude hiding (exp)

import qualified T.App.Init
import qualified T.App.Render

import qualified Opts


main :: IO ()
main = do
  cmd <- Opts.parse
  case cmd of
    Opts.Render path env ->
      T.App.Render.run path env
    Opts.Init path _env ->
      T.App.Init.run path
