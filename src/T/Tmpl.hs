{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module T.Tmpl
  ( Tmpl(..)
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
    -- | Comments
  | Comment Text
    -- | {{ exp }} context
  | Exp Exp
    -- | {% set _ = _ %}
  | Set (Ann :+ Name) Exp
    -- | {% let _ = _ %} _ {% endlet %}
  | Let (Ann :+ Name) Exp Tmpl
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
      Set name exp ->
        [ "variant" .= ("set" :: Text)
        , "name" .= name
        , "exp" .= exp
        ]
      Let name exp tmpl ->
        [ "variant" .= ("let" :: Text)
        , "name" .= name
        , "exp" .= exp
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
