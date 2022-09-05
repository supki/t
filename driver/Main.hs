{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import           Data.Foldable (traverse_)
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.IO as Text.Lazy
import           Prelude hiding (exp)
import           System.Exit (exitFailure)
import           System.IO (stderr)
import qualified Text.Trifecta as Tri
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP (AnsiStyle, hPutDoc)

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
