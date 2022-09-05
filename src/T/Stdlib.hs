{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Stdlib
  ( Stdlib
  , def
  ) where

import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (isJust)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Text.Regex.PCRE.Light as Pcre

import           T.Embed (embed)
import           T.Error (Error(..))
import           T.Exp (Name, Ann)
import           T.Exp.Ann ((:+)(..), emptyAnn, unann)
import           T.Value (Value(..), display, displayWith)


type Stdlib = HashMap Name Value

def :: HashMap Name Value
def =
  HashMap.fromList bindings
 where
  bindings =
    [ ("!" ~> flip embed not)

    , ("==" ~> eEq)
    , ("=~" ~> flip embed match)
    , ("!=" ~> eNeq)

    , ("+" ~> flip embed ((+) @Scientific))
    , ("-" ~> flip embed ((-) @Scientific))
    , ("*" ~> flip embed ((*) @Scientific))
    , ("/" ~> flip embed ((/) @Scientific))

    , (">" ~> flip embed ((>) @Scientific))
    , (">=" ~> flip embed ((>=) @Scientific))
    , ("<" ~> flip embed ((<) @Scientific))
    , ("<=" ~> flip embed ((<=) @Scientific))

    , ("empty" ~> eNull)
    , ("length" ~> eLength)

    , ("." ~> flip embed (flip (HashMap.lookup @Text @Value)))
    , ("die" ~> eDie)

    , ("show", eShow)
    , ("pp", ePP)
    ]

  name ~> binding =
    (name, binding (emptyAnn :+ name))

eEq :: Ann :+ Name -> Value
eNeq :: Ann :+ Name -> Value
(eEq, eNeq) =
  ( flip embed eq
  , flip embed neq
  )
 where
  eq x y =
    case (x, y) of
      (Null, Null) ->
        True
      (Bool b0, Bool b1) ->
        b0 == b1
      (Number n0, Number n1) ->
        n0 == n1
      (String s0, String s1) ->
        s0 == s1
      (Array arr0, Array arr1)
        | Vector.length arr0 == Vector.length arr1 -> do
            Vector.and (Vector.zipWith eq arr0 arr1)
        | otherwise ->
            False
      (Object o0, Object o1)
        | HashMap.null (HashMap.difference o0 o1) &&
          HashMap.null (HashMap.difference o1 o0) ->
            HashMap.foldl' (&&) True (HashMap.intersectionWith eq o0 o1)
        | otherwise ->
            False
      (_, _) ->
        False
  neq x y =
    not (eq x y)

match :: Text -> Pcre.Regex -> Bool
match str regexp =
  isJust (Pcre.match regexp (Text.encodeUtf8 str) [])

eNull :: Ann :+ Name -> Value
eNull (_ :+ name) =
  Lam $ \case
    ann :+ String str ->
      pure (embed (ann :+ name) (Text.null str))
    ann :+ Array xs ->
      pure (embed (ann :+ name) (null xs))
    ann :+ Object o ->
      pure (embed (ann :+ name) (HashMap.null o))
    ann :+ value ->
      Left
        (UserError
          (ann :+ name)
          ("not applicable to " <> display value <> " (not a string, array, or object)"))

eLength :: Ann :+ Name -> Value
eLength (_ :+ name) =
  Lam $ \case
    ann :+ String str ->
      pure (embed (ann :+ name) (Text.length str))
    ann :+ Array xs ->
      pure (embed (ann :+ name) (length xs))
    ann :+ Object o ->
      pure (embed (ann :+ name) (HashMap.size o))
    ann :+ value ->
      Left
        (UserError
          (ann :+ name)
          ("not applicable to " <> display value <> " (not a string, array, or object)"))

eDie :: Ann :+ Name -> Value
eDie (_ :+ name) =
  Lam (\(ann :+ val) -> Left (UserError (ann :+ name) (display val)))

eShow :: Value
eShow =
  Lam (pure . String . display . unann)

ePP :: Value
ePP =
  Lam (pure . String . displayWith Aeson.encodePretty . unann)
