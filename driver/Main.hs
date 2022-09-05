{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.IO as Text.Lazy
import           Prelude hiding (exp)
import           System.Exit (exitFailure)
import           System.IO (stderr)
import qualified Text.Trifecta as Tri
import qualified Text.Trifecta.Delta as Tri
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP (AnsiStyle, Color(..), color, hPutDoc)

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
          ppDie (docify err)
        Right (_, res) ->
          Text.Lazy.putStr res

docify :: T.Error -> PP.Doc PP.AnsiStyle
docify = \case
  T.NotInScope (Tri.Span from to line T.:+ name) ->
    Tri.prettyDelta from <> ": " <>
    PP.annotate (PP.color PP.Red) "error" <> ": " <>
    "not in scope: " <> PP.pretty name <>
    PP.line <>
    Tri.prettyRendering (Tri.addSpan from to (Tri.rendered from line))
  err ->
    PP.viaShow err

ppDie :: PP.Doc PP.AnsiStyle -> IO a
ppDie doc = do
  PP.hPutDoc stderr (doc <> PP.line)
  exitFailure
