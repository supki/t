{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.App.Render (run) where

import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as Text.Lazy

import T qualified
import T.Prelude
import T.App.IO (warn, die)
import T.App.Render.Cfg
  ( Cfg(..)
  , RenderTmpl(..)
  )


run :: Cfg -> IO ()
run cfg =
  traverse_ (runTmpl cfg.env <=< getTmpl) cfg.tmpls

getTmpl :: RenderTmpl -> IO Text
getTmpl = \case
  Path path -> do
    Text.readFile path
  String str ->
    pure (str <> "\n")

runTmpl :: T.Scope -> Text -> IO ()
runTmpl vars str =
  case T.parseText T.stdlib str of
    Left err ->
      die err
    Right tmpl ->
      case T.render (T.stdlib, vars) tmpl of
        Left err ->
          die err
        Right (warnings, res) -> do
          traverse_ warn warnings
          Text.Lazy.putStr res
