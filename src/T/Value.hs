module T.Value
  ( Value(..)
  , truthy
  , display
  , displayWith
  , typeOf
  , embedAeson
  ) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (fromHashMapText, toHashMapText)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Scientific qualified as Scientific
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Text.Regex.PCRE.Light qualified as Pcre

import T.Error (Error)
import T.Exp ((:+)(..), Ann)
import T.Prelude
import T.SExp (sexp)
import T.SExp qualified as SExp


data Value
  = Null
  | Bool Bool
  | Int Int64
  | Double Double
  | String Text
  | Regexp Pcre.Regex
  | Array (Vector Value)
  | Object (HashMap Text Value)
  | Lam (Ann :+ Value -> Either Error Value)

instance SExp.To Value where
  sexp = \case
    Null ->
      SExp.var "null"
    Bool False ->
      SExp.var "false"
    Bool True ->
      SExp.var "true"
    Int n ->
      sexp n
    Double n ->
      sexp n
    String str ->
      sexp str
    Regexp regexp ->
      SExp.round ["regexp", sexp regexp]
    Array xs ->
      SExp.square (map sexp (toList xs))
    Object xs ->
      SExp.curly
        (concatMap (\(k, v) -> [sexp k, sexp v]) (List.sortOn (\(k, _v) -> k) (HashMap.toList xs)))
    Lam _ ->
      SExp.round ["lambda", SExp.square ["_"], "..."]

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
  Text.Lazy.toStrict . Text.Lazy.decodeUtf8 . f . ejectAeson
 where
  ejectAeson = \case
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
      Aeson.Array (map ejectAeson xs)
    Object o ->
      Aeson.Object (Aeson.fromHashMapText (map ejectAeson o))
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

embedAeson :: Aeson.Value -> Value
embedAeson = \case
  Aeson.Null ->
    Null
  Aeson.Bool b ->
    Bool b
  Aeson.Number n ->
    either Double Int (Scientific.floatingOrInteger n)
  Aeson.String str ->
    String str
  Aeson.Array xs ->
    Array (map embedAeson xs)
  Aeson.Object xs ->
    Object (Aeson.toHashMapText (map embedAeson xs))
