{-# LANGUAGE OverloadedRecordDot #-}
module T.App.Init
  ( run
  ) where

import Control.Monad (foldM_, when)
import Data.Foldable (traverse_)
import Data.String (fromString)
import Data.Text.IO qualified as Text
import Prelude hiding (init, lines, writeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import T.App.Init.Cfg (Cfg(..))
import T.App.Init.Parse
  ( Stmt(..)
  , parseText
  )
import T.App.Init.IO
  ( isDirectoryNonEmpty
  , writeFile
  , userConfirm
  , warn
  , die
  )
import T qualified
import T.Render qualified


run :: Cfg -> IO ()
run cfg = do
  stmts <- parseFile cfg.init
  exec cfg stmts

parseFile :: FilePath -> IO [Stmt]
parseFile path = do
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
      testRun stmts
  when testRunSuccess $ do
    Text.putStrLn (">> Initializing: " <> fromString cfg.rootDir)
    directoryNonEmpty <- isDirectoryNonEmpty cfg.rootDir
    continue <- if directoryNonEmpty then
      userConfirm "The directory is not empty, type 'yes' to continue: "
    else
      pure True
    when continue $
      runStmts cfg.rootDir (T.mkDefEnv mempty) stmts

testRun :: [Stmt] -> IO Bool
testRun stmts =
  withSystemTempDirectory "t" $ \tmpDir -> do
    Text.putStrLn (">> Initializing: " <> fromString tmpDir <> " (test run)")
    runStmts tmpDir (T.mkDefEnv mempty) stmts
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
          pure (T.Render.mkDefEnv scope)
    File path tmpl ->
      case T.render env0 tmpl of
        Left err ->
          die (T.prettyError err)
        Right (warnings, str) -> do
          traverse_ (warn . T.prettyWarning) warnings
          let
            filepath =
              dir </> path
          Text.putStrLn (fromString filepath)
          writeFile filepath str
          pure env0
