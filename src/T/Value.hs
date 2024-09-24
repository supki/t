module T.Value
  ( Value(..)
  , truthy
  , display
  , displayWith
  , typeOf
  , reifyAeson
  ) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (fromHashMapText, toHashMapText)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Scientific qualified as Scientific
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Text.Regex.PCRE.Light qualified as Pcre

import T.Error (Error)
import T.Exp ((:+)(..), Ann)
import T.Prelude


data Value
  = Null
  | Bool Bool
  | Double Double
  | Int Int64
  | String Text
  | Array (Vector Value)
  | Object (HashMap Text Value)
  | Regexp Pcre.Regex
  | Lam (Ann :+ Value -> Either Error Value)

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
      Int value ->
        [ "variant" .= ("int" :: Text)
        , "value" .= value
        ]
      Double value ->
        [ "variant" .= ("double" :: Text)
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
        [ "variant" .= ("lambda" :: Text)
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
    Int n ->
      Aeson.Number (fromIntegral n)
    Double n ->
      Aeson.Number (Scientific.fromFloatDigits n)
    String str ->
      Aeson.String str
    Regexp _regexp ->
      Aeson.String "<regexp>"
    Array xs ->
      Aeson.Array (map embedAeson xs)
    Object o ->
      Aeson.Object (Aeson.fromHashMapText (map embedAeson o))
    Lam _f ->
      Aeson.String "<lambda>"

typeOf :: Value -> Text
typeOf = \case
  Null -> "null"
  Bool _ -> "bool"
  Int _ -> "int"
  Double _ -> "double"
  String _ -> "string"
  Regexp _ -> "regexp"
  Array _ -> "array"
  Object _ -> "object"
  Lam _ -> "lambda"

reifyAeson :: Aeson.Value -> Value
reifyAeson = \case
  Aeson.Null ->
    Null
  Aeson.Bool b ->
    Bool b
  Aeson.Number n ->
    either Double Int (Scientific.floatingOrInteger n)
  Aeson.String str ->
    String str
  Aeson.Array xs ->
    Array (map reifyAeson xs)
  Aeson.Object xs ->
    Object (Aeson.toHashMapText (map reifyAeson xs))
