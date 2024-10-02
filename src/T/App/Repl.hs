{-# LANGUAGE NamedFieldPuns #-}
module T.App.Repl where

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
  H.runInputT settings loop
 where
  loop = do
    input <- H.getInputLine "t> "
    case input of
      Nothing ->
        pure ()
      Just str0 ->
        case parseInput (fromString str0) of
          Quit ->
            pure ()
          EvalTmpl str -> do
            evalTmpl str
            loop
          ParseTmpl str -> do
            parseTmpl str
            loop

header :: IO ()
header =
  Text.putStrLn (fromString str)
 where
  str =
    printf "%s, version %s (https://github.com/supki/t)" name version
  name = Meta.name :: String
  version = Meta.version :: String

evalTmpl :: MonadIO m => Text -> m ()
evalTmpl str = liftIO $
  case T.parseText Stdlib.def str of
    Left err ->
      warn err
    Right tmpl ->
      case T.render (Stdlib.def, T.emptyScope) tmpl of
        Left err ->
          warn err
        Right (warnings, res) -> do
          traverse_ warn warnings
          Text.Lazy.putStrLn res

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