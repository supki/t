{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module T.Embed
  ( Embed(..)
  , Eject(..)
  , stdlib
  ) where

import           Data.Bool (bool)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text

import           T.Value (Value(..), display)


class Embed t where
  embed :: t -> Value

instance Embed Bool where
  embed = Bool

instance Embed Int where
  embed =
    Number . fromIntegral

instance Embed Text where
  embed = String

instance Embed a => Embed [a] where
  embed =
    Array . map embed

instance (Eject a, Embed b) => Embed (a -> b) where
  embed f =
    Lam (\x -> fmap (embed . f) (eject x))

class Eject t where
  eject :: Value -> Either String t

instance Eject Bool where
  eject = \case
    Bool bool ->
      pure bool
    value ->
      Left ("cannot eject Bool from: " <> display value)

instance Eject Text where
  eject = \case
    String str ->
      pure str
    value ->
      Left ("cannot eject Text from: " <> display value)

instance Eject a => Eject [a] where
  eject = \case
    Array xs ->
      traverse eject xs
    value ->
      Left ("cannot eject [a] from: " <> display value)

stdlib :: Value
stdlib =
  Object (HashMap.fromList bindings)
 where
  bindings =
    [ ("&&", embed (&&))
    , ("||", embed (||))
    , ("!", embed (not))

    , ("bool01", embed (bool @Int 0 1))
    , ("join", embed Text.intercalate)
    , ("split", embed Text.splitOn)
    ]
