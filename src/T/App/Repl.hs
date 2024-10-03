{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.App.Repl where

import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as Text.Lazy
import System.Directory qualified as D
import System.IO qualified as IO (hFlush, stdout)
import System.Console.Haskeline qualified as H
import Text.Printf (printf)

import Meta_t qualified as Meta
import T.App.IO (warn)
import T.Prelude
import T qualified
import T.SExp (sexp)
import T.SExp qualified as SExp
import T.Stdlib qualified as Stdlib


run :: IO ()
run = do
  header
  historyFile <- D.getXdgDirectory D.XdgState "t_history"
  let
    settings =
      H.defaultSettings
        { H.historyFile = Just historyFile
        }
  H.runInputT settings (loop T.emptyScope)
 where
  loop scope0 = do
    input <- H.getInputLine "t> "
    case input of
      Nothing ->
        pure ()
      Just str0 ->
        case parseInput (fromString str0) of
          Quit ->
            pure ()
          EvalTmpl str -> do
            scope <- evalTmpl scope0 str
            loop (fromMaybe scope0 scope)
          ParseTmpl str -> do
            parseTmpl str
            loop scope0

header :: IO ()
header =
  Text.putStrLn (fromString str)
 where
  str =
    printf "%s, version %s (https://github.com/supki/t)" name version
  name = Meta.name :: String
  version = Meta.version :: String

evalTmpl :: MonadIO m => T.Scope -> Text -> m (Maybe T.Scope)
evalTmpl scope str = liftIO $
  case T.parseText Stdlib.def str of
    Left err -> do
      warn err
      pure Nothing
    Right tmpl ->
      case T.render (Stdlib.def, scope) tmpl of
        Left err -> do
          warn err
          pure Nothing
        Right rendered -> do
          traverse_ warn rendered.warnings
          Text.Lazy.putStrLn rendered.result
          pure (Just rendered.scope)

parseTmpl :: MonadIO m => Text -> m ()
parseTmpl str = liftIO $
  case T.parseText Stdlib.def str of
    Left err ->
      warn err
    Right tmpl ->
      Text.Lazy.putStrLn (SExp.renderLazyText (sexp tmpl))

data Cmd
  = Quit
  | EvalTmpl Text
  | ParseTmpl Text
    deriving (Show, Eq)

prompt :: IO Text
prompt = do
  Text.putStr "t> "
  IO.hFlush IO.stdout
  Text.getLine

parseInput :: Text -> Cmd
parseInput input
  | Just _rest <- Text.stripPrefix ":quit" input =
    Quit
  | Just rest <- Text.stripPrefix ":eval-tmpl " input =
    EvalTmpl rest
  | Just rest <- Text.stripPrefix ":parse-tmpl " input =
    ParseTmpl rest
  | otherwise =
    EvalTmpl input
