{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.App.Init
  ( run
  ) where

import Data.Text.IO qualified as Text
import System.Directory (doesFileExist)
import System.Exit qualified as Exit (die)
import System.FilePath ((</>), (<.>))

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
import T.Prelude
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
  Text.putStrLn (">> Initializing: " <> fromString cfg.rootDir)
  directoryNonEmpty <- isDirectoryNonEmpty cfg.rootDir
  continue <- if directoryNonEmpty then
    userConfirm "The directory is not empty, type 'yes' to continue: "
  else
    pure True
  when continue $ do
    runStmts cfg.rootDir cfg.env stmts

runStmts :: FilePath -> T.Scope -> [Stmt] -> IO ()
runStmts dir =
  foldM_ (runStmt dir)

runStmt :: FilePath -> T.Scope -> Stmt -> IO T.Scope
runStmt dir scope0 stmt =
  case stmt of
    Noop ->
      pure scope0
    Setup tmpl ->
      case T.Render.exec (T.stdlib, scope0) tmpl of
        Left err ->
          die (T.prettyError err)
        Right (warnings, scope) -> do
          traverse_ (warn . T.prettyWarning) warnings
          pure scope
    File path tmpl ->
      case T.render (T.stdlib, scope0) tmpl of
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
            pure scope0
          else
            Exit.die (filepath <> " is not relative to " <> dir <> ", aborting")
