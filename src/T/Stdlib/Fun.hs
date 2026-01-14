{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Stdlib.Fun
  ( Fun(..)
  , bindings
  , typingCtx
  , functions
  ) where

import Data.Aeson.Encode.Pretty qualified as Aeson (encodePretty)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text qualified as Text
import GHC.Real (floor, ceiling, round)

import T.Embed (embed, embed0)
import T.Error (Error(..))
import T.Exp.Ann ((:+)(..), unann)
import T.Name (Name)
import T.Prelude
import T.Type (Γ, forall, fun1, fun2)
import T.Type qualified as Type
import T.Value (Value(..), display, displayWith)


data Fun = Fun
  { name     :: Name
  , ascribed :: Type.Scheme
  , binding  :: Name -> Value
  }

bindings :: [Fun] -> HashMap Name Value
bindings =
  HashMap.fromList . map (\fun -> (fun.name, fun.binding fun.name))

typingCtx :: [Fun] -> Γ
typingCtx =
  HashMap.fromList . map (\fun -> (fun.name, fun.ascribed))

functions :: [Fun]
functions =
  [ Fun "empty?"
      (forall [0] (Type.Array (Type.Var 0) `fun1` Type.Bool)) -- less polymorphic than we'd like
      nullB
  , Fun "length"
      (forall [0] (fun1 (Type.Array (Type.Var 0)) Type.Int)) -- less polymorphic than we'd like
      lengthB

  , Fun "floor"
      (forall [] (Type.Double `fun1` Type.Int))
      (embed0 (floor @Double @Int))
  , Fun "ceiling"
      (forall [] (Type.Double `fun1` Type.Int))
      (embed0 (ceiling @Double @Int))
  , Fun "round"
      (forall [] (Type.Double `fun1` Type.Int))
      (embed0 (round @Double @Int))
  , Fun "int->double"
      (forall [] (Type.Int `fun1` Type.Double))
      (embed0 (fromIntegral @Int @Double))

  , Fun "upper-case"
      (forall [] (Type.String `fun1` Type.String))
      (embed0 Text.toUpper)
  , Fun "lower-case"
      (forall [] (Type.String `fun1` Type.String))
      (embed0 Text.toLower)
  , Fun "title-case"
      (forall [] (Type.String `fun1` Type.String))
      (embed0 Text.toTitle)

  , Fun "split"
      (forall [] ((Type.String, Type.String) `fun2` Type.Array Type.String))
      (embed0 Text.splitOn)
  , Fun "join"
      (forall [] ((Type.String, Type.Array Type.String) `fun2` Type.String))
      (embed0 Text.intercalate)
  , Fun "concat"
      (forall [] (Type.Array Type.String `fun1` Type.String))
      (embed0 Text.concat)
  , Fun "chunks-of"
      (forall [] ((Type.Int, Type.String) `fun2` Type.Array Type.String))
      (embed0 Text.chunksOf)

  , Fun "die"
      (forall [0] (Type.String `fun1` Type.Var 0))
      dieB

  , Fun "show"
      (forall [0] (Type.Var 0 `fun1` Type.String))
      (embed0 showB)
  , Fun "pp"
      (forall [0] (Type.Var 0 `fun1` Type.String))
      (embed0 ppB)
  ]

nullB :: Name -> Value
nullB name =
  Lam $ \case
    ann :+ String str ->
      pure (embed (ann :+ name) (Text.null str))
    ann :+ Array xs ->
      pure (embed (ann :+ name) (List.null xs))
    ann :+ Record o ->
      pure (embed (ann :+ name) (HashMap.null o))
    ann :+ value ->
      Left
        (UserError
          (ann :+ name)
          ("not applicable to " <> display value <> " (not a string, array, or record)"))

lengthB :: Name -> Value
lengthB name =
  Lam $ \case
    ann :+ String str ->
      pure (embed (ann :+ name) (Text.length str))
    ann :+ Array xs ->
      pure (embed (ann :+ name) (List.length xs))
    ann :+ Record o ->
      pure (embed (ann :+ name) (HashMap.size o))
    ann :+ value ->
      Left
        (UserError
          (ann :+ name)
          ("not applicable to " <> display value <> " (not a string, array, or record)"))

dieB :: Name -> Value
dieB name =
  Lam (\(ann :+ val) -> Left (UserError (ann :+ name) (display val)))

showB :: Value
showB =
  Lam (pure . String . display . unann)

ppB :: Value
ppB =
  Lam (pure . String . displayWith Aeson.encodePretty . unann)
