{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Embed
  ( Embed(..)
  , Eject(..)
  , stdlib
  ) where

import           Data.Bool (bool)
import           Data.Foldable (toList)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import           T.Value (Value(..), display)


class Embed t where
  embed :: t -> Value

instance Embed Value where
  embed x = x

instance Embed Bool where
  embed = Bool

instance Embed Int where
  embed =
    Number . fromIntegral

instance Embed Scientific where
  embed = Number

instance Embed Text where
  embed = String

instance Embed a => Embed (Maybe a) where
  embed =
    maybe Null embed

instance Embed a => Embed [a] where
  embed =
    Array . Vector.fromList . map embed

instance (Eject a, Embed b) => Embed (a -> b) where
  embed f =
    Lam (\x -> fmap (embed . f) (eject x))

class Eject t where
  eject :: Value -> Either String t

instance Eject Bool where
  eject = \case
    Bool b ->
      pure b
    value ->
      Left ("cannot eject Bool from: " <> display value)

instance Eject Scientific where
  eject = \case
    Number n ->
      pure n
    value ->
      Left ("cannot eject Scientific from: " <> display value)

instance Eject Text where
  eject = \case
    String str ->
      pure str
    value ->
      Left ("cannot eject Text from: " <> display value)

instance (k ~ Text, v ~ Value) => Eject (HashMap k v) where
  eject = \case
    Object o ->
      pure o
    value ->
      Left ("cannot eject HashMap Text Value from: " <> display value)

instance Eject a => Eject [a] where
  eject = \case
    Array xs ->
      fmap toList (traverse eject xs)
    value ->
      Left ("cannot eject [a] from: " <> display value)

stdlib :: Value
stdlib =
  Object (HashMap.fromList bindings)
 where
  bindings =
    [ ("&&", embed (&&))
    , ("||", embed (||))
    , ("!", embed not)

    , ("==", eEq)
    , ("!=", eNeq)

    , ("+", embed ((+) @Scientific))
    , ("-", embed ((-) @Scientific))
    , ("*", embed ((*) @Scientific))
    , ("/", embed ((/) @Scientific))

    , ("empty", eNull)
    , ("length", eLength)

    , ("bool01", embed (bool @Int 0 1))
    , ("join", embed Text.intercalate)
    , ("split", embed Text.splitOn)

    , (".", embed (flip (HashMap.lookup @Text @Value)))
    , ("die", eDie)
    ]

eEq :: Value
eNeq :: Value
(eEq, eNeq) =
  ( Lam (\x -> pure (Lam (\y -> pure (Bool (eq x y)))))
  , Lam (\x -> pure (Lam (\y -> pure (Bool (not (eq x y))))))
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
      Left ("empty is non-applicable for: " <> display value)

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
      Left ("cannot find length of: " <> display value)

eDie :: Value
eDie =
  Lam $ \case
    value ->
      Left ("die: " <> display value)
