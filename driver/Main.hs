module Main (main) where

import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.IO as Text.Lazy
import           Prelude hiding (exp)
import           System.Exit (die)

import qualified T

import qualified Opts


main :: IO ()
main = do
  (templateName, env) <- Opts.parse
  str <- Text.readFile templateName
  case T.parse (Text.encodeUtf8 str) of
    Left err ->
      die (show err)
    Right exp ->
      case T.render env exp of
        Left err ->
          die (show err)
        Right (_, res) ->
          Text.Lazy.putStr res
