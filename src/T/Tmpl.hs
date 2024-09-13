{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module T.Tmpl
  ( Tmpl(..)
  , Assign(..)
  , (:+)(..)
  , Ann
  ) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(..))
import Data.Text (Text)
import Prelude hiding (exp)

import T.Exp.Ann (Ann, (:+)(..))
import T.Exp (Exp, Name)


infixr 1 :*:

data Tmpl
    -- | Raw template text
  = Raw Text
    -- | {# this is a comment #}
  | Comment Text
    -- | {{ exp }} context
  | Exp Exp
    -- | {% set _ = _ %}
  | Set [Assign]
    -- | {% let _ = _ %} _ {% endlet %}
  | Let [Assign] Tmpl
    -- | {% if _ %} _ {% elif _ %} _ {% else %} _ {% endif %}
  | If (NonEmpty (Exp, Tmpl))
    -- | {% for _, _ in _ %} _ {% else %} _ {% endfor %}
  | For (Ann :+ Name) (Maybe (Ann :+ Name)) Exp Tmpl (Maybe Tmpl)
    -- | Glue two `Tmpl`s together
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
      Comment str ->
        [ "variant" .= ("comment" :: Text)
        , "str" .= str
        ]
      Exp exp ->
        [ "variant" .= ("exp" :: Text)
        , "exp" .= exp
        ]
      Set assignments ->
        [ "variant" .= ("set" :: Text)
        , "assignments" .= assignments
        ]
      Let assignments tmpl ->
        [ "variant" .= ("let" :: Text)
        , "assignments" .= assignments
        , "tmpl" .= tmpl
        ]
      If clauses ->
        [ "variant" .= ("if" :: Text)
        , "clauses" .= clauses
        ]
      For name it exp forTmpl elseTmpl ->
        [ "variant" .= ("for" :: Text)
        , "name" .= name
        , "it" .= it
        , "exp" .= exp
        , "for" .= forTmpl
        , "else" .= elseTmpl
        ]
      tmpl0 :*: tmpl1 ->
        [ "variant" .= (":*:" :: Text)
        , "tmpl0" .= tmpl0
        , "tmpl1" .= tmpl1
        ]

data Assign = Assign (Ann :+ Name) Exp
    deriving (Show, Eq)

instance Aeson.ToJSON Assign where
  toJSON (Assign name exp) =
    Aeson.object
      [ "name" .= name
      , "exp" .= exp
      ]
