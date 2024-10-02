module T.App.IO
  ( warn
  , die
  , ppWarn
  , ppDie
  ) where

import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP (AnsiStyle, hPutDoc)
import System.Exit (exitFailure)
import System.IO (stderr)

import T.Prelude
import T qualified


class Warn t where
  warn :: t -> IO ()

instance Warn T.Warning where
  warn =
    ppWarn . T.prettyWarning

instance Warn T.Error where
  warn =
    ppWarn . T.prettyError

instance Warn T.ParseError where
  warn (T.ParseError err) =
    ppWarn err

die :: Warn t => t -> IO ()
die err = do
  warn err
  exitFailure

ppWarn :: PP.Doc PP.AnsiStyle -> IO ()
ppWarn doc =
  PP.hPutDoc stderr (doc <> PP.line)

ppDie :: PP.Doc PP.AnsiStyle -> IO a
ppDie doc = do
  ppWarn doc
  exitFailure
