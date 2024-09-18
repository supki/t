{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.App.Init
  ( run
  ) where

import Control.Monad (foldM_, when, unless)
import Data.Foldable (traverse_)
import Data.String (fromString)
import Data.Text.IO qualified as Text
import Prelude hiding (init, lines, writeFile)
import System.Directory (doesFileExist)
import System.Exit qualified as Exit (die)
import System.FilePath ((</>), (<.>))
import System.IO.Temp (withSystemTempDirectory)

import T.App.Init.Cfg
  ( Cfg(..)
  , InitTmpl(..)
  )
import T.App.Init.Parse
  ( Stmt(..)
  , parseText
  )
import T.App.Init.IO
  ( isDirectoryNonEmpty
  , writeFile
  , isRelativeTo
  , userConfirm
  , warn
  , die
  )
import T qualified
import T.Render qualified


run :: Cfg -> IO ()
run cfg = do
  tmpl <- findTmpl cfg
  stmts <- parseTmpl tmpl
  exec cfg stmts

findTmpl :: Cfg -> IO FilePath
findTmpl cfg = do
  let
    path = case cfg.tmpl of
      Name name ->
        cfg.tmplDir </> name <.> "t"
      Path path0 ->
        path0
  exists <- doesFileExist path
  unless exists . die $
    "Couldn't find the template at " <> fromString path
  pure path

parseTmpl :: FilePath -> IO [Stmt]
parseTmpl path = do
  str <- Text.readFile path
  case parseText str of
    Left err ->
      die (fromString err)
    Right stmts ->
      pure stmts

exec :: Cfg -> [Stmt] -> IO ()
exec cfg stmts = do
  testRunSuccess <-
    if cfg.skipTestRun then
      pure True
    else
      testRun cfg stmts
  when testRunSuccess $ do
    Text.putStrLn (">> Initializing: " <> fromString cfg.rootDir)
    directoryNonEmpty <- isDirectoryNonEmpty cfg.rootDir
    continue <- if directoryNonEmpty then
      userConfirm "The directory is not empty, type 'yes' to continue: "
    else
      pure True
    when continue $ do
      runStmts cfg.rootDir cfg.env stmts

testRun :: Cfg -> [Stmt] -> IO Bool
testRun cfg stmts =
  withSystemTempDirectory "t" $ \tmpDir -> do
    Text.putStrLn (">> Initializing: " <> fromString tmpDir <> " (test run)")
    runStmts tmpDir cfg.env stmts
    userConfirm "Init finished, take a look, and then type 'yes' to continue: "

runStmts :: FilePath -> T.Env -> [Stmt] -> IO ()
runStmts dir =
  foldM_ (runStmt dir)

runStmt :: FilePath -> T.Env -> Stmt -> IO T.Env
runStmt dir env0 stmt =
  case stmt of
    Noop ->
      pure env0
    Setup tmpl ->
      case T.Render.exec env0 tmpl of
        Left err ->
          die (T.prettyError err)
        Right (warnings, scope) -> do
          traverse_ (warn . T.prettyWarning) warnings
          pure (T.mkDefEnv scope)
    File path tmpl ->
      case T.render env0 tmpl of
        Left err ->
          die (T.prettyError err)
        Right (warnings, str) -> do
          traverse_ (warn . T.prettyWarning) warnings
          let
            filepath =
              dir </> path
          relative <- filepath `isRelativeTo` dir
          if relative then do
            Text.putStrLn (fromString filepath)
            writeFile filepath str
            pure env0
          else
            Exit.die (filepath <> " is not relative to " <> dir <> ", aborting")
