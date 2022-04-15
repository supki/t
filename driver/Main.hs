module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import           Data.String (fromString)
import qualified Data.Text.Lazy.IO as Text.Lazy
import           Prelude hiding (exp)
import           System.Environment (getArgs)
import           System.Exit (die)

import qualified T


main :: IO ()
main = do
  templateName : envStr : _ <- getArgs
  str <- ByteString.readFile templateName
  case T.parse str of
    Left err ->
      die err
    Right exp ->
      case envParse envStr of
        Left err ->
          die err
        Right env ->
          case T.render env exp of
            Left err ->
              die err
            Right res ->
              Text.Lazy.putStrLn res

envParse :: String -> Either String T.Env
envParse =
  fmap T.Env . Aeson.eitherDecode . fromString
