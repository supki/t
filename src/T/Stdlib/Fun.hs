{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Stdlib.Fun
  ( Fun(..)
  , bindings
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
import T.Value (Value(..), display, displayWith)


data Fun = Fun
  { name    :: Name
  , binding :: Name -> Value
  }

bindings :: [Fun] -> [(Name, Value)]
bindings =
  map (\fun -> (fun.name, fun.binding fun.name))

functions :: [Fun]
functions =
  [ Fun "empty?" nullB
  , Fun "length" lengthB

  , Fun "floor" (flip embed0 (floor @Double @Int64))
  , Fun "ceiling" (flip embed0 (ceiling @Double @Int64))
  , Fun "round" (flip embed0 (round @Double @Int64))
  , Fun "int->double" (flip embed0 (fromIntegral @Int64 @Double))

  , Fun "upper-case" (flip embed0 Text.toUpper)
  , Fun "lower-case" (flip embed0 Text.toLower)
  , Fun "title-case" (flip embed0 Text.toTitle)

  , Fun "split" (flip embed0 Text.splitOn)
  , Fun "join" (flip embed0 Text.intercalate)
  , Fun "concat" (flip embed0 Text.concat)

  , Fun "." (flip embed0 (flip (HashMap.lookup @Text @Value)))
  , Fun "die" dieB

  , Fun "show" (\_ -> showB)
  , Fun "pp" (\_ -> ppB)
  ]

nullB :: Name -> Value
nullB name =
  Lam $ \case
    ann :+ String str ->
      pure (embed (ann :+ name) (Text.null str))
    ann :+ Array xs ->
      pure (embed (ann :+ name) (List.null xs))
    ann :+ Object o ->
      pure (embed (ann :+ name) (HashMap.null o))
    ann :+ value ->
      Left
        (UserError
          (ann :+ name)
          ("not applicable to " <> display value <> " (not a string, array, or object)"))

lengthB :: Name -> Value
lengthB name =
  Lam $ \case
    ann :+ String str ->
      pure (embed (ann :+ name) (Text.length str))
    ann :+ Array xs ->
      pure (embed (ann :+ name) (List.length xs))
    ann :+ Object o ->
      pure (embed (ann :+ name) (HashMap.size o))
    ann :+ value ->
      Left
        (UserError
          (ann :+ name)
          ("not applicable to " <> display value <> " (not a string, array, or object)"))

dieB :: Name -> Value
dieB name =
  Lam (\(ann :+ val) -> Left (UserError (ann :+ name) (display val)))

showB :: Value
showB =
  Lam (pure . String . display . unann)

ppB :: Value
ppB =
  Lam (pure . String . displayWith Aeson.encodePretty . unann)
