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
  , varE
  , varE_
  , ifE
  , ifE_
  , appE
  , appE_
  , falseL
  , trueL
  ) where

import Data.HashMap.Strict qualified as HashMap
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
  | Var (Ann :+ Name)
  | If a a a
  | App (Ann :+ Name) (NonEmpty a)
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

instance Eq1 ExpF where
  liftEq _ (Lit l0) (Lit l1) =
    l0 == l1
  liftEq _ (Var v0) (Var v1) =
    v0 == v1
  liftEq (==?) (If p0 t0 f0) (If p1 t1 f1) =
    (p0 ==? p1) && (t0 ==? t1) && (f0 ==? f1)
  liftEq (==?) (App n0 as0) (App n1 as1) =
    (n0 == n1) && List.and (NonEmpty.zipWith (==?) as0 as1)
  liftEq _ _ _ =
    False

litE :: Ann -> Literal -> Exp
litE ann lit =
  ann :< Lit lit

litE_ :: Literal -> Exp
litE_ =
  litE emptyAnn

varE :: Ann -> Ann :+ Name -> Exp
varE ann name =
  ann :< Var name

varE_ :: Ann :+ Name -> Exp
varE_ =
  varE emptyAnn

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

data Literal
  = Null
  | Bool Bool
  | Int Int64
  | Double Double
  | String Text
  | Regexp Pcre.Regex
  | Array (Vector Exp)
  | Object (HashMap Text Exp)
    deriving (Show, Eq)

instance SExp.To Literal where
  sexp = \case
    Null ->
      SExp.var "null"
    Bool False ->
      SExp.var "false"
    Bool True ->
      SExp.var "true"
    Int n ->
      sexp n
    Double n ->
      sexp n
    String str ->
      sexp str
    Regexp regexp ->
      SExp.round ["regexp", sexp regexp]
    Array xs ->
      SExp.square (map sexp (toList xs))
    Object xs ->
      SExp.curly
        (concatMap (\(k, v) -> [sexp k, sexp v]) (List.sortOn (\(k, _v) -> k) (HashMap.toList xs)))

falseL :: Literal
falseL =
  Bool False

trueL :: Literal
trueL =
  Bool True
