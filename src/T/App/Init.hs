{-# LANGUAGE OverloadedRecordDot #-}
module T.App.Init
  ( run
  ) where

import Control.Monad (foldM_, when)
import Data.Foldable (traverse_)
import Data.String (fromString)
import Data.Text.IO qualified as Text
import Prelude hiding (lines, writeFile)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

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


run :: FilePath -> IO ()
run path = do
  dir <- getCurrentDirectory
  stmts <- parseFile path
  exec dir stmts

parseFile :: FilePath -> IO [Stmt]
parseFile path = do
  str <- Text.readFile path
  case parseText str of
    Left err ->
      die (fromString err)
    Right stmts ->
      pure stmts

exec :: FilePath -> [Stmt] -> IO ()
exec dir stmts0 = do
  doRealRun <- withSystemTempDirectory "t" $ \tmpDir -> do
    Text.putStrLn ("Test run in: " <> fromString tmpDir)
    runStmts tmpDir (T.mkDefEnv mempty) stmts0
    Text.putStrLn "Test run finished, type 'yes' to continue: "
    userConfirm
  when doRealRun $ do
    Text.putStrLn ("Test run in: " <> fromString dir)
    filesExist <- isDirectoryNonEmpty dir
    continue <- if filesExist then do
      Text.putStrLn "The directory is not empty, type 'yes' to continue: "
      userConfirm
    else
      pure True
    when continue $
      runStmts dir (T.mkDefEnv mempty) stmts0

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
          writeFile (dir </> path) str
          pure env0
