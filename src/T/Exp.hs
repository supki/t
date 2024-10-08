{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module T.Exp
  ( Exp
  , Cofree(..)
  , ExpF(..)
  , Literal(..)
  , (:+)(..)
  , Ann
  , litE
  , litE_
  , false
  , true
  , int
  , array
  , record
  , varE
  , ifE
  , ifE_
  , appE
  , appE_
  , idxE
  , idxE_
  , keyE_
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Text.Regex.PCRE.Light qualified as Pcre

import T.Exp.Ann (Ann, (:+)(..), emptyAnn)
import T.Name (Name(..))
import T.Prelude
import T.SExp (sexp)
import T.SExp qualified as SExp


data Cofree f a = a :< f (Cofree f a)

deriving instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a)

instance Eq1 f => Eq (Cofree f a) where
  (_ :< f) == (_ :< g) =
    eq1 f g

type Exp = Cofree ExpF Ann

data ExpF a
  = Lit Literal
    -- ^ literals: 4, [1,2,3], {foo: 4}
  | Var (Ann :+ Name)
    -- ^ variable lookup: foo
  | If a a a
    -- ^ if-expression: if ... then ... else ...
  | App (Ann :+ Name) (NonEmpty a)
    -- ^ application: f(x)
  | Idx a a
    -- ^ array index access: xs[0]
  | Key a (Ann :+ Name)
    -- ^ record property access: foo.bar
    deriving (Show, Eq, Generic1)

instance SExp.To Exp where
  sexp = \case
    _ :< Lit lit ->
      sexp lit
    _ :< Var (_ :+ name) ->
      sexp name
    _ :< If p t f ->
      SExp.round ["if", sexp p, sexp t, sexp f]
    _ :< App (_ :+ name) args ->
      SExp.round (sexp name : map sexp (toList args))
    _ :< Idx exp expIdx ->
      SExp.round ["at-index", sexp expIdx, sexp exp]
    _ :< Key exp key ->
      SExp.round ["at-key", sexp key, sexp exp]

instance Eq1 ExpF where
  liftEq _ (Lit l0) (Lit l1) =
    l0 == l1
  liftEq _ (Var v0) (Var v1) =
    v0 == v1
  liftEq (==?) (If p0 t0 f0) (If p1 t1 f1) =
    (p0 ==? p1) && (t0 ==? t1) && (f0 ==? f1)
  liftEq (==?) (App n0 as0) (App n1 as1) =
    (n0 == n1) && List.and (NonEmpty.zipWith (==?) as0 as1)
  liftEq (==?) (Idx e0 idx0) (Idx e1 idx1) =
    (e0 ==? e1) && (idx0 ==? idx1)
  liftEq (==?) (Key e0 key0) (Key e1 key1) =
    (e0 ==? e1) && (key0 == key1)
  liftEq _ _ _ =
    False

litE :: Ann -> Literal -> Exp
litE ann lit =
  ann :< Lit lit

litE_ :: Literal -> Exp
litE_ =
  litE emptyAnn

false :: Exp
false =
  litE_ (Bool False)

true :: Exp
true =
  litE_ (Bool True)

int :: Int -> Exp
int =
  litE_ . Int

array :: Vector Exp -> Exp
array =
  litE_ . Array

record :: HashMap Name Exp -> Exp
record =
  litE_ . Record

varE :: Ann :+ Name -> Exp
varE name@(ann :+ _) =
  ann :< Var name

ifE :: Ann -> Exp -> Exp -> Exp -> Exp
ifE ann p t f =
  ann :< If p t f

ifE_ :: Exp -> Exp -> Exp -> Exp
ifE_ =
  ifE emptyAnn

appE :: Ann -> Ann :+ Name -> NonEmpty Exp -> Exp
appE ann name args =
  ann :< App name args

appE_ :: Ann :+ Name -> NonEmpty Exp -> Exp
appE_ =
  appE emptyAnn

idxE :: Ann -> Exp -> Exp -> Exp
idxE ann exp idx =
  ann :< Idx exp idx

idxE_ :: Exp -> Exp -> Exp
idxE_ =
  idxE emptyAnn

keyE_ :: Exp -> Ann :+ Name -> Exp
keyE_ exp key =
  emptyAnn :< Key exp key

data Literal
  = Null
  | Bool Bool
  | Int Int
  | Double Double
  | String Text
  | Regexp Pcre.Regex
  | Array (Vector Exp)
  | Record (HashMap Name Exp)
    deriving (Show, Eq)

instance SExp.To Literal where
  sexp = \case
    Null ->
      SExp.var "null"
    Bool b ->
      sexp b
    Int n ->
      sexp n
    Double n ->
      sexp n
    String str ->
      sexp str
    Regexp regexp ->
      SExp.round ["regexp", sexp regexp]
    Array xs ->
      sexp xs
    Record xs ->
      sexp xs
