module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.String (fromString)
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.IO as Text.Lazy
import           Prelude hiding (exp)
import           System.Exit (die)

import qualified T

import qualified Opts


main :: IO ()
main = do
  (templateName, envStr) <- Opts.parse
  str <- Text.readFile templateName
  case T.parse (Text.encodeUtf8 str) of
    Left err ->
      die (show err)
    Right exp ->
      case envParse envStr of
        Left err ->
          die (show err)
        Right Nothing ->
          die ("not a JSON object: " <> envStr)
        Right (Just env) ->
          case T.render env exp of
            Left err ->
              die (show err)
            Right (_, res) ->
              Text.Lazy.putStr res

envParse :: String -> Either String (Maybe T.Env)
envParse =
  fmap (T.mkDefEnv . HashMap.mapKeys fromString) . Aeson.eitherDecode . fromString
