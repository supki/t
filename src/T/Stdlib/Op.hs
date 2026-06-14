{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
module T.Stdlib.Op
  ( Op(..)
  , PriorityMap
  , Fixity(..)
  , bindings
  , typingCtx
  , priorities
  , operators
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Text.Regex.PCRE.Light qualified as Pcre

import T.Embed (embed0)
import T.Error (Error(..))
import T.Exp (varE)
import T.Exp.Ann ((:+)(..))
import T.Name (Name)
import T.Prelude
import T.SExp (sexp)
import T.Type (Γ, forAll, forAll_, fun1, fun2, tyVar)
import T.Type qualified as Type
import T.Value (Value(..), typeOf)


data Op = Op
  { name     :: Name
  , ascribed :: Type.Scheme
  , binding  :: Name -> Value
  , fixity   :: Fixity
  , priority :: Int
  }

type PriorityMap = Map Int [(Name, Fixity)]

data Fixity
  = Prefix
  | Infix
  | Infixl
  | Infixr
    deriving (Show, Eq)

bindings :: [Op] -> HashMap Name Value
bindings =
  HashMap.fromList . map (\op -> (op.name, op.binding op.name))

typingCtx :: [Op] -> Γ
typingCtx =
  HashMap.fromList . map (\op -> (op.name, op.ascribed))

priorities :: [Op] -> PriorityMap
priorities =
  Map.fromListWith (<>) . map (\op -> (op.priority, [(op.name, op.fixity)]))

operators :: [Op]
operators =
  [ Op "!"
      (forAll_ (Type.Bool `fun1` Type.Bool))
      (embed0 not) Prefix 8

  , Op "=="
      (forAll [0] [(0, Type.Eq)] ((tyVar 0, tyVar 0) `fun2` Type.Bool))
      (embed0 eq) Infix 4
  , Op "!="
      (forAll [0] [(0, Type.Eq)] ((tyVar 0, tyVar 0) `fun2` Type.Bool))
      (embed0 neq) Infix 4
  , Op "=~"
      (forAll_ ((Type.String, Type.Regexp) `fun2` Type.Bool))
      (embed0 match) Infix 4

  , Op "+"
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` tyVar 0))
      add Infixl 6
  , Op "-"
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` tyVar 0))
      subtract Infixl 6
  , Op "*"
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` tyVar 0))
      multiply Infixl 7
  , Op "/"
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` tyVar 0))
      divide Infixl 7

  , Op "<"
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` Type.Bool))
      lt Infix 4
  , Op "<="
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` Type.Bool))
      le Infix 4
  , Op ">"
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` Type.Bool))
      gt Infix 4
  , Op ">="
      (forAll [0] [(0, Type.Num)] ((tyVar 0, tyVar 0) `fun2` Type.Bool))
      ge Infix 4

  , Op "<>"
      (forAll_ ((Type.String, Type.String) `fun2` Type.String))
      (embed0 ((<>) @Text)) Infixr 6
  ]

combineNumbers :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Name -> Value
combineNumbers intOp doubleOp name =
  Lam $ \case
    _ann :+ Int n0 ->
      pure . Lam $ \case
        _ann :+ Int n1 ->
          pure (Int (n0 `intOp` n1))
        ann :+ n ->
          Left (TagMismatch (varE (ann :+ name)) Type.Int (typeOf n) (sexp n))
    _ann :+ Double n0 ->
      pure . Lam $ \case
        _ann :+ Double n1 ->
          pure (Double (n0 `doubleOp` n1))
        ann :+ n ->
          Left (TagMismatch (varE (ann :+ name)) Type.Double (typeOf n) (sexp n))
    ann :+ n ->
      Left (TagMismatch (varE (ann :+ name)) (Type.Var 0 [Type.Num]) (typeOf n) (sexp n))

add :: Name -> Value
add =
  combineNumbers (+) (+)

subtract :: Name -> Value
subtract =
  combineNumbers (-) (-)

multiply :: Name -> Value
multiply =
  combineNumbers (*) (*)

divide :: Name -> Value
divide =
  combineNumbers div (/)

predicateNumbers :: (Int -> Int -> Bool) -> (Double -> Double -> Bool) -> Name -> Value
predicateNumbers intOp doubleOp name =
  Lam $ \case
    _ann :+ Int n0 ->
      pure . Lam $ \case
        _ann :+ Int n1 ->
          pure (Bool (n0 `intOp` n1))
        ann :+ n ->
          Left (TagMismatch (varE (ann :+ name)) Type.Int (typeOf n) (sexp n))
    _ann :+ Double n0 ->
      pure . Lam $ \case
        _ann :+ Double n1 ->
          pure (Bool (n0 `doubleOp` n1))
        ann :+ n ->
          Left (TagMismatch (varE (ann :+ name)) Type.Double (typeOf n) (sexp n))
    ann :+ n ->
      Left (TagMismatch (varE (ann :+ name)) (Type.Var 0 [Type.Num]) (typeOf n) (sexp n))

lt :: Name -> Value
lt =
  predicateNumbers (<) (<)

le :: Name -> Value
le =
  predicateNumbers (<=) (<=)

gt :: Name -> Value
gt =
  predicateNumbers (>) (>)

ge :: Name -> Value
ge =
  predicateNumbers (>=) (>=)

eq :: Value -> Value -> Bool
eq x y =
  case (x, y) of
    (Null, Null) ->
      True
    (Bool b0, Bool b1) ->
      b0 == b1
    (Int n0, Int n1) ->
      n0 == n1
    (Double n0, Double n1) ->
      n0 == n1
    (String s0, String s1) ->
      s0 == s1
    (Array arr0, Array arr1)
      | Vector.length arr0 == Vector.length arr1 -> do
          Vector.and (Vector.zipWith eq arr0 arr1)
      | otherwise ->
          False
    (Record o0, Record o1)
      | HashMap.null (HashMap.difference o0 o1) &&
        HashMap.null (HashMap.difference o1 o0) ->
          HashMap.foldl' (&&) True (HashMap.intersectionWith eq o0 o1)
      | otherwise ->
          False
    (_, _) ->
      False

neq :: Value -> Value -> Bool
neq x y =
  not (eq x y)

match :: Text -> Pcre.Regex -> Bool
match str regexp =
  isJust (Pcre.match regexp (Text.encodeUtf8 str) [])
