{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy.IO qualified as Text.Lazy
import Prelude hiding (exp)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Trifecta qualified as Tri
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP (AnsiStyle, hPutDoc)

import qualified T

import qualified Opts


main :: IO ()
main = do
  (path, env) <- Opts.parse
  str <- Text.readFile path
  case T.parseFile path (Text.encodeUtf8 str) of
    Left Tri.ErrInfo {Tri._errDoc} ->
      ppDie _errDoc
    Right exp ->
      case T.render env exp of
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
