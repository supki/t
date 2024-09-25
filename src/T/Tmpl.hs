{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module T.Tmpl
  ( Tmpl(..)
  , Assign(..)
  , (:+)(..)
  , Ann
  ) where

import T.Exp (Exp)
import T.Exp.Ann (Ann, (:+)(..))
import T.Name (Name)
import T.Prelude
import T.SExp (sexp)
import T.SExp qualified as SExp


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
    -- | Glue `Tmpl`s together
  | Cat [Tmpl]
    deriving (Show, Eq)

instance IsString Tmpl where
  fromString =
    Raw . fromString

instance SExp.To Tmpl where
  sexp = \case
    Raw str ->
      SExp.round ["raw", sexp str]
    Comment str ->
      SExp.round ["comment", sexp str]
    Exp exp ->
      SExp.round ["exp", sexp exp]
    Set assigns ->
      SExp.round ["set", SExp.square (map sexp assigns)]
    Let assigns tmpl ->
      SExp.round ["let", SExp.square (map sexp assigns), sexp tmpl]
    If cases ->
      SExp.round ["if", SExp.square (map (\(exp, tmpl) -> SExp.square [sexp exp, sexp tmpl]) (toList cases))]
    For value keyQ exp tmpl elseTmpl ->
      SExp.round
        [ "for"
        , SExp.square [sexp value, maybe (SExp.var "_key") sexp keyQ]
        , sexp exp
        , sexp tmpl
        , maybe (SExp.var "_else") sexp elseTmpl
        ]
    Cat tmpls ->
      SExp.square (map sexp tmpls)

data Assign = Assign (Ann :+ Name) Exp
    deriving (Show, Eq)

instance SExp.To Assign where
  sexp (Assign (_ :+ name) exp) =
    SExp.square [sexp name, sexp exp]
