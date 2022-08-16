{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Stdlib
  ( def
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


def :: HashMap Name Value
def =
  HashMap.fromList bindings
 where
  bindings =
    [ ("!", embed not)

    , ("==", eEq)
    , ("=~", embed match)
    , ("!=", eNeq)

    , ("+", embed ((+) @Scientific))
    , ("-", embed ((-) @Scientific))
    , ("*", embed ((*) @Scientific))
    , ("/", embed ((/) @Scientific))

    , ("empty", eNull)
    , ("length", eLength)

    , (".", embed (flip (HashMap.lookup @Text @Value)))
    , ("die", eDie)

    , ("show", eShow)
    , ("pp", ePP)
    ]

eEq :: Value
eNeq :: Value
(eEq, eNeq) =
  ( embed eq
  , embed neq
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

eNull :: Value
eNull =
  Lam $ \case
    String str ->
      pure (embed (Text.null str))
    Array xs ->
      pure (embed (null xs))
    Object o ->
      pure (embed (HashMap.null o))
    value ->
      Left (GenericError ("empty is non-applicable for: " <> display value))

eLength :: Value
eLength =
  Lam $ \case
    String str ->
      pure (embed (Text.length str))
    Array xs ->
      pure (embed (length xs))
    Object o ->
      pure (embed (HashMap.size o))
    value ->
      Left (GenericError ("cannot find length of: " <> display value))

eDie :: Value
eDie =
  Lam $ \case
    value ->
      Left (GenericError ("die: " <> display value))

eShow :: Value
eShow =
  Lam (pure . String . display)

ePP :: Value
ePP =
  Lam (pure . String . displayWith Aeson.encodePretty)
