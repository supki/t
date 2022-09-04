{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Stdlib
  ( Stdlib
  , def
  ) where

import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (isJust)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Text.Regex.PCRE.Light as Pcre

import           T.Embed (embed)
import           T.Error (Error(..))
import           T.Exp (Name)
import           T.Value (Value(..), display, displayWith)


type Stdlib = HashMap Name Value

def :: HashMap Name Value
def =
  HashMap.fromList bindings
 where
  bindings =
    [ ("!" ~> flip embed not)

    , ("==" ~> eEq)
    , ("=~" ~> flip embed match)
    , ("!=" ~> eNeq)

    , ("+" ~> flip embed ((+) @Scientific))
    , ("-" ~> flip embed ((-) @Scientific))
    , ("*" ~> flip embed ((*) @Scientific))
    , ("/" ~> flip embed ((/) @Scientific))

    , (">" ~> flip embed ((>) @Scientific))
    , (">=" ~> flip embed ((>=) @Scientific))
    , ("<" ~> flip embed ((<) @Scientific))
    , ("<=" ~> flip embed ((<=) @Scientific))

    , ("empty" ~> eNull)
    , ("length" ~> eLength)

    , ("." ~> flip embed (flip (HashMap.lookup @Text @Value)))
    , ("die" ~> eDie)

    , ("show", eShow)
    , ("pp", ePP)
    ]

  name ~> binding =
    (name, binding name)

eEq :: Name -> Value
eNeq :: Name -> Value
(eEq, eNeq) =
  ( flip embed eq
  , flip embed neq
  )
 where
  eq x y =
    case (x, y) of
      (Null, Null) ->
        True
      (Bool b0, Bool b1) ->
        b0 == b1
      (Number n0, Number n1) ->
        n0 == n1
      (String s0, String s1) ->
        s0 == s1
      (Array arr0, Array arr1)
        | Vector.length arr0 == Vector.length arr1 -> do
            Vector.and (Vector.zipWith eq arr0 arr1)
        | otherwise ->
            False
      (Object o0, Object o1)
        | HashMap.null (HashMap.difference o0 o1) &&
          HashMap.null (HashMap.difference o1 o0) ->
            HashMap.foldl' (&&) True (HashMap.intersectionWith eq o0 o1)
        | otherwise ->
            False
      (_, _) ->
        False
  neq x y =
    not (eq x y)

match :: Text -> Pcre.Regex -> Bool
match str regexp =
  isJust (Pcre.match regexp (Text.encodeUtf8 str) [])

eNull :: Name -> Value
eNull name =
  Lam $ \case
    String str ->
      pure (embed name (Text.null str))
    Array xs ->
      pure (embed name (null xs))
    Object o ->
      pure (embed name (HashMap.null o))
    value ->
      Left
        (UserError
          name
          ("not applicable to " <> display value <> " (not a string, array, or object)"))

eLength :: Name -> Value
eLength name =
  Lam $ \case
    String str ->
      pure (embed name (Text.length str))
    Array xs ->
      pure (embed name (length xs))
    Object o ->
      pure (embed name (HashMap.size o))
    value ->
      Left
        (UserError
          name
          ("not applicable to " <> display value <> " (not a string, array, or object)"))

eDie :: Name -> Value
eDie name =
  Lam (Left . UserError name . display)

eShow :: Value
eShow =
  Lam (pure . String . display)

ePP :: Value
ePP =
  Lam (pure . String . displayWith Aeson.encodePretty)
