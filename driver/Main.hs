{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.IO as Text.Lazy
import           Prelude hiding (exp)
import           System.Exit (exitFailure)
import           System.IO (stderr)
import           Text.Trifecta (ErrInfo(..))
import qualified Prettyprinter as PP (Doc, line, viaShow)
import qualified Prettyprinter.Render.Terminal as PP (AnsiStyle, hPutDoc)

import qualified T

import qualified Opts


main :: IO ()
main = do
  (path, env) <- Opts.parse
  str <- Text.readFile path
  case T.parse (Text.encodeUtf8 str) of
    Left ErrInfo {_errDoc} ->
      ppDie _errDoc
    Right exp ->
      case T.render env exp of
        Left err ->
          ppDie (PP.viaShow err)
        Right (_, res) ->
          Text.Lazy.putStr res

ppDie :: PP.Doc PP.AnsiStyle -> IO a
ppDie doc = do
  PP.hPutDoc stderr (doc <> PP.line)
  exitFailure
