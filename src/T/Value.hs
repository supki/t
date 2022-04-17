module T.Value
  ( Value(..)
  , display
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import           Data.Vector (Vector)


data Value
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Array (Vector Value)
  | Object (HashMap Text Value)
  | Lam (Value -> Either String Value)

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

display :: Value -> String
display =
  Text.Lazy.unpack . Text.Lazy.decodeUtf8 . Aeson.encode . embedAeson
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
    Array xs ->
      Aeson.Array (fmap embedAeson xs)
    Object o ->
      Aeson.Object (fmap embedAeson o)
    Lam _f ->
      Aeson.String "<lambda>"
