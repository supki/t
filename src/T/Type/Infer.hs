{-# LANGUAGE OverloadedRecordDot #-}
module T.Type.Infer
  ( infer
  ) where

import Control.Monad (foldM)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Data.HashMap.Strict qualified as HashMap

import T.Exp (Exp, (:+)(..))
import T.Exp qualified as Exp
import T.Name (Name)
import T.Prelude
import T.Tmpl (Tmpl(..))
import T.Type.Vocab
  ( InferenceT
  , runInferenceT
  , Γ
  , Σ(..)
  , emptyΣ
  , freshVar
  , TypeError(..)
  , TypedExp
  , Type(..)
  , Scheme(..)
  )
import T.Type.Unify
  ( unify
  , replaceOnce
  , finalize
  )


infer :: Γ -> Exp -> Either TypeError TypedExp
infer ctx exp = do
  (te, finalΣ) <- runIdentity (runInferenceT ctx emptyΣ (inferExp exp))
  pure (finalize finalΣ.subst te)

inferTmpl :: Monad m => Tmpl -> InferenceT m ()
inferTmpl = \case
  Raw _text ->
    pure ()
  Comment _text ->
    pure ()
  Exp exp -> do
    _texp <- inferExp exp
    pure ()

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

checkKey :: Monad m => TypedExp -> Name -> InferenceT m Type
checkKey r name = do
  case extractType r of
    Record fields ->
      case HashMap.lookup name fields of
        Just t ->
          pure t
        Nothing ->
          throwError (MissingKey name)
    var@(Var _n _cs) -> do
      v <- freshVar
      _ <- unify var (Record (HashMap.singleton name v))
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
    t <- case toList (map extractType ys) of
      [] ->
        freshVar
      z : zs ->
        foldM unify z zs
    pure (Array t)
  Exp.Record r -> do
    ts <- traverse inferExp r
    pure (Record (map extractType ts))

lookupCtx :: Monad m => Name -> InferenceT m Type
lookupCtx name = do
  ctx <- ask
  maybe (throwError (MissingVar name)) instantiate (HashMap.lookup name ctx)

instantiate :: Monad m => Scheme -> InferenceT m Type
instantiate (Forall qs t) = do
  fvs <- for (toList qs) $ \q -> do
    fv <- freshVar
    pure (q, fv)
  pure (replaceOnce (HashMap.fromList fvs) t)

extractType :: TypedExp -> Type
extractType ((_ann, t) :< _e) = t
