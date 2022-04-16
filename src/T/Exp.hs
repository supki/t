{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module T.Exp
  ( Tmpl(..)
  , Exp(..)
  , Literal(..)
  , Name(..)
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.Foldable (toList)
import           Data.HashMap.Strict (HashMap)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Scientific (Scientific)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (exp)


infixr 1 :*:

data Tmpl
    -- ^ Raw template text
  = Raw Text
    -- ^ {{ exp }} context
  | Exp Exp
    -- ^ {% set _ = _ %}
  | Set Name Exp
    -- ^ {% if _ %} _ {% elif _ %} _ {% else %} _ {% endif %}
  | If (NonEmpty (Exp, Tmpl))
    -- ^ Glue two `Tmpl`s together
  | Tmpl :*: Tmpl
    deriving (Show, Eq)

instance IsString Tmpl where
  fromString =
    Raw . fromString

instance Aeson.ToJSON Tmpl where
  toJSON =
    Aeson.object . \case
      Raw str ->
        [ "variant" .= ("raw" :: Text)
        , "str" .= str
        ]
      Exp exp ->
        [ "variant" .= ("exp" :: Text)
        , "exp" .= exp
        ]
      Set name exp ->
        [ "variant" .= ("set" :: Text)
        , "name" .= name
        , "exp" .= exp
        ]
      If clauses ->
        [ "variant" .= ("if" :: Text)
        , "clauses" .= clauses
        ]
      tmpl0 :*: tmpl1 ->
        [ "variant" .= (":*:" :: Text)
        , "tmpl0" .= tmpl0
        , "tmpl1" .= tmpl1
        ]

data Exp 
  = Lit Literal
  | Var Name
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
  | Array [Exp]
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
      Array value ->
        [ "variant" .= ("array" :: Text)
        , "value" .= value
        ]
      Object value ->
        [ "variant" .= ("object" :: Text)
        , "value" .= value
        ]
  
newtype Name = Name { unName :: NonEmpty Text }
    deriving (Show, Eq)

instance Aeson.ToJSON Name where
  toJSON =
    Aeson.toJSON . Text.intercalate "." . toList . unName
