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
  , Ann(..)
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
inferExp (ann0 :< e) =
  case e of
    Exp.Lit l -> do
      t <- inferLiteral l
      pure (annOf t :< Exp.Lit l)

    Exp.Var (ann :+ name) -> do
      t <- lookupCtx name
      pure (annOf t :< Exp.Var (Ann {spanned = ann, typed = t} :+ name))

    Exp.If b0 t0 f0 -> do
      b1@(tb :< _) <- inferExp b0
      _ <- unify tb.typed Bool
      t1@(tt :< _) <- inferExp t0
      f1@(tf :< _) <- inferExp f0
      tu <- unify tt.typed tf.typed
      pure (annOf tu :< Exp.If b1 t1 f1)

    Exp.App (ann :+ name) args0 -> do
      args1 <- traverse inferExp args0
      t <- checkApp name args1
      pure (annOf t :< Exp.App (Ann {spanned = ann, typed = t} :+ name) args1)

    Exp.Idx arr0 idx0 -> do
      arr1 <- inferExp arr0
      idx1 <- inferExp idx0
      t <- checkIdx arr1 idx1
      pure (annOf t :< Exp.Idx arr1 idx1)

    Exp.Key r0 (ann :+ name) -> do
      r1 <- inferExp r0
      t <- checkKey r1 name
      pure (annOf t :< Exp.Key r1 (Ann {spanned = ann, typed = t} :+ name))
 where
  annOf inferred =
    Ann {spanned = ann0, typed = inferred}

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
extractType (ann :< _e) = ann.typed
