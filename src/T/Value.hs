{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module T.Value
  ( Value(..)
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific (Scientific)
import           Data.Text (Text)


data Value
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Array [Value]
  | Object (HashMap Text Value)
    deriving (Show, Eq)

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
