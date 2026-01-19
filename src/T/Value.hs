module T.Value
  ( Value(..)
  , truthy
  , display
  , displayWith
  , typeOf
  , embedAeson
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (fromHashMapText, toHashMapText)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Scientific qualified as Scientific
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Text.Regex.PCRE.Light qualified as Pcre

import T.Error (Error)
import T.Exp ((:+)(..), Ann)
import T.Name (Name(..))
import T.Name qualified as Name
import T.Prelude
import T.SExp (sexp)
import T.SExp qualified as SExp
import T.Type (Type)
import T.Type qualified as Type


data Value
  = Null
  | Bool Bool
  | Int Int
  | Double Double
  | String Text
  | Regexp Pcre.Regex
  | Array (Vector Value)
  | Record (HashMap Name Value)
  | Lam (Ann :+ Value -> Either Error Value)

instance SExp.To Value where
  sexp = \case
    Null ->
      SExp.var "null"
    Bool b ->
      sexp b
    Int n ->
      sexp n
    Double n ->
      sexp n
    String str ->
      sexp str
    Regexp regexp ->
      SExp.round ["regexp", sexp regexp]
    Array xs ->
      sexp xs
    Record xs ->
      sexp xs
    Lam _ ->
      SExp.round ["lambda", SExp.square ["_"], "..."]

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
    Record r ->
      Aeson.Object (Aeson.fromHashMapText (HashMap.mapKeys Name.toText (map ejectAeson r)))
    Lam _f ->
      Aeson.String "<lambda>"

typeOf :: Value -> Type
typeOf = \case
  Null -> Type.Unit
  Bool _ -> Type.Bool
  Int _ -> Type.Int
  Double _ -> Type.Double
  String _ -> Type.String
  Regexp _ -> Type.Regexp
  Array _ ->
    Type.Array (error "element")
  Record fields ->
    Type.Record (map typeOf fields)
  Lam _ ->
    Type.Fun (error "args") (error "result")

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
    Record (HashMap.mapKeys Name (Aeson.toHashMapText (map embedAeson xs)))
