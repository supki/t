module T.App.Init.IO
  ( isDirectoryNonEmpty
  , writeFile
  , isRelativeTo
  , userConfirm
  , die
  , warn
  ) where

import Control.Exception (catch, throwIO)
import Data.List qualified as List
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy.IO qualified as Text.Lazy
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP (AnsiStyle, hPutDoc)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , listDirectory
  , renameFile
  )
import System.Exit (exitFailure)
import System.FilePath (splitDirectories, takeDirectory)
import System.IO (hFlush, stderr, stdout)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (emptySystemTempFile)

import T.Prelude


isDirectoryNonEmpty :: FilePath -> IO Bool
isDirectoryNonEmpty =
  map (not . List.null . filter (`notElem` [".", ".."])) . listDirectory

-- | Write to a temporary file first and then move it to the desired location.
writeFile :: FilePath -> Lazy.Text -> IO ()
writeFile dst str = do
  tmpSrc <- emptySystemTempFile "t"
  Text.Lazy.writeFile tmpSrc str
  moveFile tmpSrc dst

-- | Move file from A to B, creating any missing directories in the process.
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dst =
  renameFile src dst
 `catch` \exc ->
  if isDoesNotExistError exc then do
    createDirectoryIfMissing True (takeDirectory dst)
    renameFile src dst
  else
    throwIO exc

isRelativeTo :: FilePath -> FilePath -> IO Bool
isRelativeTo long0 short0 = do
  long <- canonicalizePath long0
  short <- canonicalizePath short0
  pure (go (splitDirectories long) (splitDirectories short))
 where
  go (x : xs) (y : ys)
    | x == y =
      go xs ys
    | otherwise = False
  go (_ : _) [] =
    True
  go _ _ =
    False

userConfirm :: Text -> IO Bool
userConfirm msg = do
  Text.putStr msg
  hFlush stdout
  reply <- Text.getLine
  pure (reply == "yes")

die :: PP.Doc PP.AnsiStyle -> IO a
die doc = do
  warn doc
  exitFailure

warn :: PP.Doc PP.AnsiStyle -> IO ()
warn doc =
  PP.hPutDoc stderr (doc <> PP.line)
