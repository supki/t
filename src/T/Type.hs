{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.Type
  ( Type(..)
  , infer
  , TypeError
  ) where

import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.State (StateT, MonadState, runStateT, gets, modify)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Data.List.NonEmpty qualified as NonEmpty
import Data.HashMap.Strict qualified as HashMap

import T.Exp (Exp, (:+)(..))
import T.Exp qualified as Exp
import T.Name (Name)
import T.Prelude


data Type
  = Unit
  | Bool
  | Int
  | Double
  | String
  | Regexp
  | Array Type
  | Record (HashMap Name Type)
  | Fun (NonEmpty Type) Type
  | Var Int
    deriving (Show, Eq)

type TypedExp = Cofree Exp.ExpF (Exp.Ann, Type)

newtype InferenceT m a = InferenceT (ReaderT Γ (StateT Σ (ExceptT TypeError m)) a)
    deriving (Functor, Applicative, Monad, MonadReader Γ, MonadState Σ, MonadError TypeError)

type Γ = HashMap Name Type

data Σ = Σ
  { subst   :: Subst
  , counter :: Int
  } deriving (Show, Eq)

type Subst = HashMap Int Type

emptyΣ :: Σ
emptyΣ = Σ
  { subst = mempty
  , counter = 0
  }

data TypeError
  = MissingKey Name
  | MissingVar Name
  | NotARecord Type
  | TypeMismatch Type Type
  | OccursCheck Int Type

runInferenceT :: Γ -> Σ -> InferenceT m a -> m (Either TypeError (a, Σ))
runInferenceT ctx subst (InferenceT m) =
  runExceptT (runStateT (runReaderT m ctx) subst)

infer :: Exp -> Either TypeError TypedExp
infer exp = do
  (te, Σ {subst}) <- runIdentity (runInferenceT mempty emptyΣ (inferExp exp))
  pure (finalize subst te)

inferExp :: Monad m => Exp -> InferenceT m TypedExp
inferExp (ann :< e) = do
  te <- traverse inferExp e
  inferredType <- case te of
    Exp.Lit l ->
      inferLiteral l
    Exp.Var (_ann :+ name) ->
      lookupCtx name
    Exp.If b t f -> do
      _ <- unify (extractType b) Bool
      unify (extractType t) (extractType f)
    Exp.App (_ann :+ name) args ->
      checkApp name args
    Exp.Idx arr idx ->
      checkIdx arr idx
    Exp.Key r (_ann :+ name) ->
      checkKey r name
  pure ((ann, inferredType) :< te)

lookupCtx :: Monad m => Name -> InferenceT m Type
lookupCtx name = do
  ctx <- ask
  maybe (throwError (MissingVar name)) pure (HashMap.lookup name ctx)

extractType :: TypedExp -> Type
extractType ((_ann, t) :< _e) = t

unify :: Monad m => Type -> Type -> InferenceT m Type
unify t1 t2 = do
  s <- gets (.subst)
  let
    t1' =
      applySubst s t1
    t2' =
      applySubst s t2
  case (t1', t2') of
    (a, b)
      | a == b ->
        pure a
    (Var n, t) -> do
      when (occurs n t s) $
        throwError (OccursCheck n t)
      extendSubst n t
      pure t
    (t, Var n) -> do
      when (occurs n t s) $
        throwError (OccursCheck n t)
      extendSubst n t
      pure t
    (Array a, Array b) ->
      map Array (unify a b)
    (Record m1, Record m2) -> do
      map Record (sequence (HashMap.intersectionWith unify m1 m2))
    (Fun args1 ret1, Fun args2 ret2) ->
      liftA2 Fun (sequence (NonEmpty.zipWith unify args1 args2)) (unify ret1 ret2)
    _ ->
      throwError (TypeMismatch t1' t2')

occurs :: Int -> Type -> Subst -> Bool
occurs n t subst =
  case applySubst subst t of
    Var m ->
      n == m
    Array arr ->
      occurs n arr subst
    Record r ->
      any (\t' -> occurs n t' subst) r
    Fun args r ->
      any (\a -> occurs n a subst) args || occurs n r subst
    _ ->
      False

applySubst :: Subst -> Type -> Type
applySubst subst t =
  case t of
    Var n ->
      case HashMap.lookup n subst of
        Just nt ->
          applySubst subst nt
        Nothing ->
          Var n
    Array arr ->
      Array (applySubst subst arr)
    Record r ->
      Record (map (applySubst subst) r)
    Fun args r ->
      Fun (map (applySubst subst) args) (applySubst subst r)
    Unit ->
      Unit
    Bool ->
      Bool
    Int ->
      Int
    Double ->
      Double
    String ->
      String
    Regexp ->
      Regexp

extendSubst :: Monad m => Int -> Type -> InferenceT m ()
extendSubst n t =
  modify (\s -> s { subst = HashMap.insert n t s.subst })

checkApp :: Monad m => Name -> NonEmpty TypedExp -> InferenceT m Type
checkApp name args = do
  ft <- lookupCtx name
  r <- freshVar
  _ <- unify ft (Fun (map extractType args) r)
  pure r

checkIdx :: Monad m => TypedExp -> TypedExp -> InferenceT m Type
checkIdx arr idx = do
  e <- freshVar
  _ <- unify (extractType arr) (Array e)
  _ <- unify (extractType idx) Int
  pure e

freshVar :: Monad m => InferenceT m Type
freshVar = do
  n <- gets (.counter)
  modify (\s -> s { counter = s.counter + 1 })
  pure (Var n)

checkKey :: Monad m => TypedExp -> Name -> InferenceT m Type
checkKey r name = do
  case extractType r of
    Record fields ->
      case HashMap.lookup name fields of
        Just t ->
          pure t
        Nothing ->
          throwError (MissingKey name)
    Var n -> do
      v <- freshVar
      _ <- unify (Var n) (Record (HashMap.singleton name v))
      pure v
    _ ->
      throwError (NotARecord (extractType r))

inferLiteral :: Monad m => Exp.Literal -> InferenceT m Type
inferLiteral = \case
  Exp.Null -> pure Unit
  Exp.Bool _ -> pure Bool
  Exp.Int _ -> pure Int
  Exp.Double _ -> pure Double
  Exp.String _ -> pure String
  Exp.Regexp _ -> pure Regexp
  Exp.Array xs -> do
    ys <- traverse inferExp xs
    y <- generalize (toList (map extractType ys))
    pure (Array y)
  Exp.Record r -> do
    ts <- traverse inferExp r
    pure (Record (map extractType ts))

generalize :: Monad m => [Type] -> InferenceT m Type
generalize = \case
  [] -> freshVar
  t : ts ->
    foldM unify t ts

finalize :: Subst -> TypedExp -> TypedExp
finalize subst cofree =
  map (\(ann, t) -> (ann, applySubst subst t)) cofree
