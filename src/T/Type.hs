{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.Type
  ( Γ
  , Type(..)
  , Scheme(..)
  , Constraint(..)
  , infer
  , TypeError(..)
  -- * stdlib helpers
  , forAll
  , forAll_
  , fun1
  , fun2
  , tyVar
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set

import T.Prelude
import T.Type.Vocab
  ( InferenceT(..)
  , Γ
  , TypeError(..)
  , Type(..)
  , freeVarsType
  , Scheme(..)
  , freeVarsScheme
  , Constraint(..)
  )
import T.Type.Infer (infer)


forAll :: [Int] -> [(Int, Constraint)] -> Type -> Scheme
forAll qs cs t = do
  let
    cm =
      HashMap.fromListWith Set.union (map (\(i, c) -> (i, Set.singleton c)) cs)
  Forall (Set.fromList qs) (injectConstraints cm t)

injectConstraints :: HashMap Int (Set Constraint) -> Type -> Type
injectConstraints cm t = case t of
  Var n cs ->
    Var n (cs <> HashMap.findWithDefault Set.empty n cm)
  Array arr ->
    Array (injectConstraints cm arr)
  Record r ->
    Record (map (injectConstraints cm) r)
  Fun args r ->
    Fun (map (injectConstraints cm) args) (injectConstraints cm r)
  _ ->
    t

forAll_ :: Type -> Scheme
forAll_ =
  forAll [] []

fun1 :: Type -> Type -> Type
fun1 a1 r =
  Fun (a1 :| []) r

fun2 :: (Type, Type) -> Type -> Type
fun2 (a1, a2) r =
  Fun (a1 :| a2 : []) r

tyVar :: Int -> Type
tyVar n =
  Var n mempty

generalize :: Monad m => Γ -> Type -> InferenceT m Scheme
generalize ctx t = do
  let
    fvs =
      freeVarsType t
    ctxvs =
      foldMap freeVarsScheme ctx
    qs =
      Set.difference fvs ctxvs
  pure (Forall qs t)
