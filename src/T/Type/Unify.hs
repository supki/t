{-# LANGUAGE OverloadedRecordDot #-}
module T.Type.Unify
  ( unify
  , replace
  , replaceOnce
  , finalize
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (gets, modify)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set

import T.Prelude
import T.Type.Vocab
  ( InferenceT
  , Σ(..)
  , Subst
  , TypedExp
  , Ann(..)
  , Type(..)
  , TypeError(..)
  , Constraint
  , satisfies
  )


unify :: Monad m => Type -> Type -> InferenceT m Type
unify t1 t2 = do
  s <- gets (.subst)
  case (replace s t1, replace s t2) of
    (a, b)
      | a == b ->
        pure a
    (Var n cs, t) -> do
      unifyVar s n cs t
    (t, Var n cs) -> do
      unifyVar s n cs t
    (Array a, Array b) ->
      map Array (unify a b)
    (Record m1, Record m2) -> do
      map Record (sequence (HashMap.intersectionWith unify m1 m2))
    (Fun args1 ret1, Fun args2 ret2)
      | NonEmpty.length args1 == NonEmpty.length args2 ->
        liftA2 Fun (sequence (NonEmpty.zipWith unify args1 args2)) (unify ret1 ret2)
    (a, b) ->
      throwError (TypeMismatch a b)

unifyVar :: Monad m => Subst -> Int -> Set Constraint -> Type -> InferenceT m Type
unifyVar s n cs t0 = do
  when (occurs n t0 s) (throwError (OccursCheck n t0))
  checkConstraints cs t0
  let
    t =
      applyConstraints cs t0
  extendSubst n t
  pure t

replace :: Subst -> Type -> Type
replace subst t =
  case t of
    Array arr ->
      Array (replace subst arr)
    Record r ->
      Record (map (replace subst) r)
    Fun args r ->
      Fun (map (replace subst) args) (replace subst r)
    Var n cs ->
      case HashMap.lookup n subst of
        Nothing ->
          Var n cs
        Just nt ->
          applyConstraints cs (replace subst nt)
    _ ->
      t

-- | 'replaceOnce' is a variant of 'replace' that doesn't
-- do deep substitution; this is necessary separate the namespaces
-- of quantified variables and unitification variables which is
-- useful for e.g. stdlib definitions
replaceOnce :: Subst -> Type -> Type
replaceOnce subst t =
  case t of
    Array arr ->
      Array (replaceOnce subst arr)
    Record r ->
      Record (map (replaceOnce subst) r)
    Fun args r ->
      Fun (map (replaceOnce subst) args) (replaceOnce subst r)
    Var n cs ->
      case HashMap.lookup n subst of
        Nothing ->
          Var n cs
        Just (Var m ds) ->
          Var m (Set.union cs ds)
        Just x ->
          applyConstraints cs x
    _ ->
      t

occurs :: Int -> Type -> Subst -> Bool
occurs n t subst =
  case replace subst t of
    Var m _cs ->
      n == m
    Array arr ->
      occurs n arr subst
    Record r ->
      any (\t' -> occurs n t' subst) r
    Fun args r ->
      any (\a -> occurs n a subst) args || occurs n r subst
    _ ->
      False

checkConstraints :: Monad m => Set Constraint -> Type -> InferenceT m ()
checkConstraints cs t = do
  for_ cs $ \c ->
    unless (satisfies c t) (throwError (ConstraintViolation c t))

applyConstraints :: Set Constraint -> Type -> Type
applyConstraints cs = \case
  Var n vcs ->
    Var n (Set.union vcs cs)
  t ->
    t

extendSubst :: Monad m => Int -> Type -> InferenceT m ()
extendSubst n t =
  modify (\s -> s {subst = HashMap.insert n t s.subst})

finalize :: Subst -> TypedExp -> TypedExp
finalize subst =
  map (\ann -> ann {typed = defaultType (replace subst ann.typed)})

defaultType :: Type -> Type
defaultType = \case
  Var {} ->
    Unit
  Array t ->
    Array (defaultType t)
  Record r ->
    Record (map defaultType r)
  t ->
    t
