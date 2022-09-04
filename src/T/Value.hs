module T.Value
  ( Value(..)
  , truthy
  , display
  , displayWith
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import           Data.Vector (Vector)
import qualified Text.Regex.PCRE.Light as Pcre

import           T.Error (Error)


data Value
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Array (Vector Value)
  | Object (HashMap Text Value)
  | Regexp Pcre.Regex
  | Lam (Value -> Either Error Value)

instance Aeson.ToJSON Value where
  toJSON =
    Aeson.object . \case
      Null ->
        [ "variant" .= ("null" :: Text)
        ]
      Bool value ->
        [ "variant" .= ("bool" :: Text)
        , "value" .= value
        ]
      Number value ->
        [ "variant" .= ("number" :: Text)
        , "value" .= value
        ]
      String value ->
        [ "variant" .= ("string" :: Text)
        , "value" .= value
        ]
      Regexp value ->
        [ "variant" .= ("regexp" :: Text)
        , "value" .= show value
        ]
      Array value ->
        [ "variant" .= ("array" :: Text)
        , "value" .= value
        ]
      Object value ->
        [ "variant" .= ("object" :: Text)
        , "value" .= value
        ]
      Lam _f ->
        [ "variant" .= ("<lambda>" :: Text)
        ]

truthy :: Value -> Bool
truthy = \case
  Null -> False
  Bool False -> False
  _ -> True

display :: Value -> Text
display =
  displayWith Aeson.encode

displayWith :: (Aeson.Value -> Lazy.ByteString) -> Value -> Text
displayWith f =
  Text.Lazy.toStrict . Text.Lazy.decodeUtf8 . f . embedAeson
 where
  embedAeson = \case
    Null ->
      Aeson.Null
    Bool b ->
      Aeson.Bool b
    Number n ->
      Aeson.Number n
    String str ->
      Aeson.String str
    Regexp _regexp ->
      Aeson.String "<regexp>"
    Array xs ->
      Aeson.Array (fmap embedAeson xs)
    Object o ->
      Aeson.Object (fmap embedAeson o)
    Lam _f ->
      Aeson.String "<lambda>"
