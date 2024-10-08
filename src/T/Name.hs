{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.Name
  ( Name(..)
  , toString
  , toText
  ) where


import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Prettyprinter (Pretty(..))

import T.Prelude


newtype Name = Name { unName :: Text }
    deriving
      ( Show
      , Eq
      , Ord
      , IsString
      , Hashable
      , Aeson.FromJSONKey
      , Aeson.ToJSON
      , Aeson.ToJSONKey
      )

instance Pretty Name where
  pretty name =
    pretty name.unName

toString :: Name -> String
toString name =
  Text.unpack name.unName

toText :: Name -> Text
toText name =
  name.unName
