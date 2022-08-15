{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module T.Exp
  ( Exp(..)
  , Literal(..)
  , Name(..)
  , (:<)(..)
  , Ann
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Scientific (Scientific)
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Prelude hiding (exp)
import qualified Text.Regex.PCRE.Light as Pcre

import           T.Exp.Ann (Ann, (:<)(..))


data Exp 
  = Lit Literal
  | Var (Ann :< Name)
  | If Exp Exp Exp
  | App Exp Exp
    deriving (Show, Eq)

instance Aeson.ToJSON Exp where
  toJSON =
    Aeson.object . \case
      Lit value ->
        [ "variant" .= ("lit" :: Text)
        , "value" .= value
        ]
      Var name ->
        [ "variant" .= ("var" :: Text)
        , "name" .= name
        ]
      If p expt expf ->
        [ "variant" .= ("if" :: Text)
        , "predicate" .= p
        , "when-true" .= expt
        , "when-false" .= expf
        ]
      App exp0 exp1 ->
        [ "variant" .= ("app" :: Text)
        , "exp0" .= exp0
        , "exp1" .= exp1
        ]

data Literal
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Regexp Pcre.Regex
  | Array (Vector Exp)
  | Object (HashMap Text Exp)
    deriving (Show, Eq)

instance Aeson.ToJSON Literal where
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
      Regexp value ->
        [ "variant" .= ("regexp" :: Text)
        , "value" .= show value
        ]
      Array value ->
        [ "variant" .= ("array" :: Text)
        , "value" .= value
        ]
      Object value ->
        [ "variant" .= ("object" :: Text)
        , "value" .= value
        ]
  
newtype Name = Name { unName :: Text }
    deriving (Show, Eq, IsString, Hashable)

instance Aeson.ToJSON Name where
  toJSON =
    Aeson.toJSON . unName
