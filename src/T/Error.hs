module T.Error
  ( Error(..)
  , prettyError
  , Warning(..)
  , prettyWarning
  ) where

import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP (Color(..), color)
import Text.Trifecta qualified as Tri
import Text.Trifecta.Delta qualified as Tri

import T.Exp (Cofree((:<)), Exp, ExpF(..), (:+)(..), Ann)
import T.Name (Name)
import T.Prelude
import T.Type (Type)


data Error
  = NotInScope (Ann :+ Name)
    -- Ideally, we want a Value here instead of Text,
    -- but there's neither Show Value nor Eq Value,
    -- and that makes working with Error annoying too.
  | OutOfBounds Exp Text Text
  | MissingProperty Exp Text Text
  | UserError (Ann :+ Name) Text
  | TypeError Exp Type Type Text
    deriving (Show, Eq)

prettyError :: Error -> Doc AnsiStyle
prettyError = \case
  NotInScope (ann :+ name) ->
    header ann <>
    "not in scope: " <> PP.pretty name <> PP.line <>
    excerpt ann
  OutOfBounds (ann :< _) array idx ->
    header ann <>
    "index: " <> PP.pretty idx <> PP.line <>
    "is out of bounds for array: " <> PP.pretty array <> PP.line <>
    excerpt ann
  MissingProperty (ann :< _) r key ->
    header ann <>
    "key: " <> PP.pretty key <> PP.line <>
    "is missing from the record: " <> PP.pretty r <> PP.line <>
    excerpt ann
  UserError (ann :+ name) text ->
    header ann <>
    PP.pretty name <> ": " <> PP.pretty text <> PP.line <>
    excerpt ann
  TypeError (ann :< Var (_ann :+ name)) expected actual value ->
    header ann <>
    "mismatched types in " <> PP.pretty name <> ": " <> PP.line <>
      PP.indent 2 "expected: " <> PP.pretty (show expected) <> PP.line <>
      PP.indent 2 " but got: " <> PP.pretty value <> " : " <> PP.pretty (show actual) <> PP.line <>
    excerpt ann
  TypeError (ann :< _) expected actual value ->
    header ann <>
      PP.indent 2 "expected: " <> PP.pretty (show expected) <> PP.line <>
      PP.indent 2 " but got: " <> PP.pretty value <> " : " <> PP.pretty (show actual) <> PP.line <>
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
