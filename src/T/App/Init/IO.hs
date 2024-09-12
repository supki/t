module T.App.Init.IO
  ( isDirectoryNonEmpty
  , writeFile
  , userConfirm
  , die
  , warn
  ) where

import Control.Exception (catch, throwIO)
import Data.String (fromString)
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy.IO qualified as Text.Lazy
import Prelude hiding (writeFile)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP (AnsiStyle, hPutDoc)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)


isDirectoryNonEmpty :: FilePath -> IO Bool
isDirectoryNonEmpty =
  fmap (not . null . filter (`notElem` [".", ".."])) . listDirectory

writeFile :: FilePath -> Lazy.Text -> IO ()
writeFile path str = do
  Text.putStrLn ("Creating: " <> fromString path)
  Text.Lazy.writeFile path str
 `catch` \exc ->
  if isDoesNotExistError exc then do
    createDirectoryIfMissing True (takeDirectory path)
    Text.Lazy.writeFile path str
  else
    throwIO exc

userConfirm :: IO Bool
userConfirm = do
  reply <- Text.getLine
  pure (reply == "yes")

die :: PP.Doc PP.AnsiStyle -> IO a
die doc = do
  warn doc
  exitFailure

warn :: PP.Doc PP.AnsiStyle -> IO ()
warn doc =
  PP.hPutDoc stderr (doc <> PP.line)
