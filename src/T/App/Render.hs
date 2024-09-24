{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.App.Render (run) where

import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy.IO qualified as Text.Lazy
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Trifecta qualified as Tri
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP (AnsiStyle, hPutDoc)

import T qualified
import T.Prelude
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
  case T.parse T.stdlib (Text.encodeUtf8 str) of
    Left Tri.ErrInfo {Tri._errDoc} ->
      ppDie _errDoc
    Right exp ->
      case T.render (T.stdlib, vars) exp of
        Left err ->
          ppDie (T.prettyError err)
        Right (warnings, res) -> do
          traverse_ (ppWarn . T.prettyWarning) warnings
          Text.Lazy.putStr res

ppDie :: PP.Doc PP.AnsiStyle -> IO a
ppDie doc = do
  ppWarn doc
  exitFailure

ppWarn :: PP.Doc PP.AnsiStyle -> IO ()
ppWarn doc =
  PP.hPutDoc stderr (doc <> PP.line)
