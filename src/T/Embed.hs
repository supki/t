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

import           T.Value (Value(..), display)


class Embed t where
  embed :: t -> Value

instance Embed Bool where
  embed = Bool

instance Embed Int where
  embed =
    Number . fromIntegral

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

stdlib :: Value
stdlib =
  Object (HashMap.fromList bindings)
 where
  bindings =
    [ ("&&", embed (&&))
    , ("||", embed (||))
    , ("!", embed (not))

    , ("bool01", embed (bool @Int 0 1))
    ]
