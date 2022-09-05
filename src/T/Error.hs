module T.Error
  ( Error(..)
  , Warning(..)
  , prettyError
  ) where

import           Data.Text (Text)
import           Prettyprinter (Doc)
import qualified Prettyprinter as PP
import           Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as PP (Color(..), color)
import qualified Text.Trifecta as Tri
import qualified Text.Trifecta.Delta as Tri

import           T.Exp (Cofree((:<)), Exp, (:+)(..), Name, Ann)


data Error
  = NotInScope (Ann :+ Name)
    -- Ideally, we want a Value here instead of Text,
    -- but there's neither Show Value nor Eq Value,
    -- and that makes working with Error annoying too.
  | NotIterable Exp Text
  | NotRenderable Exp Text
  | NotAFunction (Ann :+ Name) Text
  | UserError Name Text
  | TypeError Name Text Text
    deriving (Show, Eq)

prettyError :: Error -> Doc AnsiStyle
prettyError = \case
  NotInScope (ann :+ name) ->
    header ann <>
    "not in scope: " <> PP.pretty name <> PP.line <>
    excerpt ann
  NotIterable (ann :< _) value ->
    header ann <>
    "not an iterable: " <> PP.pretty value <> PP.line <>
    excerpt ann
  NotRenderable (ann :< _) value ->
    header ann <>
    "not renderable: " <> PP.pretty value <> PP.line <>
    excerpt ann
  NotAFunction (ann :+ name) value ->
    header ann <>
    "not a function: " <> PP.pretty name <> PP.line <>
      PP.indent 2 "but something else: " <> PP.pretty value <> PP.line <>
    excerpt ann
  err ->
    PP.viaShow err
 where
  header (Tri.Span from _to _line) =
    Tri.prettyDelta from <> ": " <>
    PP.annotate (PP.color PP.Red) "error" <> ": "
  excerpt (Tri.Span from to line) =
    Tri.prettyRendering (Tri.addSpan from to (Tri.rendered from line))

data Warning
  = ShadowedBy ((Ann, Ann) :+ Name)
    deriving (Show, Eq)
