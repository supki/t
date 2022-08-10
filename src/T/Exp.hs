{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module T.Exp
  ( Tmpl(..)
  , Exp(..)
  , Literal(..)
  , Name(..)
  , (:<)(..)
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

import           T.Exp.Ann (Span, (:<)(..), unann)


infixr 1 :*:

data Tmpl
    -- ^ Raw template text
  = Raw Text
    -- ^ {{ exp }} context
  | Exp Exp
    -- ^ {% set _ = _ %}
  | Set (Span :< Name) Exp
    -- ^ {% let _ = _ %} _ {% endlet %}
  | Let (Span :< Name) Exp Tmpl
    -- ^ {% if _ %} _ {% elif _ %} _ {% else %} _ {% endif %}
  | If (NonEmpty (Exp, Tmpl))
    -- ^ {% for _, _ in _ %} _ {% else %} _ {% endfor %}
  | For (Span :< Name) (Maybe (Span :< Name)) Exp Tmpl (Maybe Tmpl)
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
        , "name" .= unann name
        , "exp" .= exp
        ]
      Let name exp tmpl ->
        [ "variant" .= ("let" :: Text)
        , "name" .= unann name
        , "exp" .= exp
        , "tmpl" .= tmpl
        ]
      If clauses ->
        [ "variant" .= ("if" :: Text)
        , "clauses" .= clauses
        ]
      For name it exp forTmpl elseTmpl ->
        [ "variant" .= ("for" :: Text)
        , "name" .= unann name
        , "it" .= fmap unann it
        , "exp" .= exp
        , "for" .= forTmpl
        , "else" .= elseTmpl
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
