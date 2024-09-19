{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.Name
  ( Name(..)
  , toString
  ) where


import Data.Aeson qualified as Aeson
import Data.Hashable (Hashable)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (exp)
import Prettyprinter (Pretty(..))


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
