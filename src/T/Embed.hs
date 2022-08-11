{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Embed
  ( Embed(..)
  , Eject(..)
  ) where

import           Data.Foldable (toList)
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Text.Regex.PCRE.Light as Pcre

import           T.Error (Error(..))
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
  eject :: Value -> Either Error t

instance Eject Value where
  eject = pure

instance Eject Bool where
  eject = \case
    Bool b ->
      pure b
    value ->
      Left (GenericError ("cannot eject Bool from: " <> display value))

instance Eject Scientific where
  eject = \case
    Number n ->
      pure n
    value ->
      Left (GenericError ("cannot eject Scientific from: " <> display value))

instance Eject Text where
  eject = \case
    String str ->
      pure str
    value ->
      Left (GenericError ("cannot eject Text from: " <> display value))

instance Eject Pcre.Regex where
  eject = \case
    Regexp regexp ->
      pure regexp
    value ->
      Left (GenericError ("cannot eject Pcre.Regex from: " <> display value))

instance (k ~ Text, v ~ Value) => Eject (HashMap k v) where
  eject = \case
    Object o ->
      pure o
    value ->
      Left (GenericError ("cannot eject HashMap Text Value from: " <> display value))

instance Eject a => Eject [a] where
  eject = \case
    Array xs ->
      fmap toList (traverse eject xs)
    value ->
      Left (GenericError ("cannot eject [a] from: " <> display value))
