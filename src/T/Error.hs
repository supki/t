module T.Error
  ( Error(..)
  , prettyError
  , Warning(..)
  , prettyWarning
  ) where

import Data.Text (Text)
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP (Color(..), color)
import Text.Trifecta qualified as Tri
import Text.Trifecta.Delta qualified as Tri

import T.Exp (Cofree((:<)), Exp, (:+)(..), Name, Ann)


data Error
  = NotInScope (Ann :+ Name)
    -- Ideally, we want a Value here instead of Text,
    -- but there's neither Show Value nor Eq Value,
    -- and that makes working with Error annoying too.
  | NotIterable Exp Text
  | NotRenderable Exp Text
  | NotAFunction (Ann :+ Name) Text
  | UserError (Ann :+ Name) Text
  | TypeError (Ann :+ Name) Text Text Text
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
  UserError (ann :+ name) text ->
    header ann <>
    PP.pretty name <> ": " <> PP.pretty text <> PP.line <>
    excerpt ann
  TypeError (ann :+ name) expected actual value ->
    header ann <>
    "mismatched types in " <> PP.pretty name <> ": " <> PP.line <>
      PP.indent 2 "expected: something convertable to " <> PP.pretty expected <> PP.line <>
      PP.indent 2 " but got: " <> PP.pretty value <> " : " <> PP.pretty actual <> PP.line <>
    excerpt ann
 where
  header (Tri.Span from _to _line) =
    Tri.prettyDelta from <> ": " <>
    PP.annotate (PP.color PP.Red) "error" <> ": "

data Warning
  = ShadowedBy ((Ann, Ann) :+ Name)
    deriving (Show, Eq, Ord)

prettyWarning :: Warning -> Doc AnsiStyle
prettyWarning = \case
  ShadowedBy ((shadowed, shadower) :+ name) ->
    header shadower <>
    "shadowed binding: " <> PP.pretty name <> PP.line <>
    "first defined at: " <> PP.line <>
    excerpt shadowed <> PP.line <>
    "but then redefined at: " <> PP.line <>
    excerpt shadower
 where
  header (Tri.Span from _to _line) =
    Tri.prettyDelta from <> ": " <>
    PP.annotate (PP.color PP.Yellow) "warning" <> ": "

excerpt :: Tri.Span -> Doc AnsiStyle
excerpt (Tri.Span from to line) =
  Tri.prettyRendering (Tri.addSpan from to (Tri.rendered from line))
