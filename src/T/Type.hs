{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.Type
  ( Γ
  , Type(..)
  , Scheme(..)
  , forall
  , fun1
  , fun2
  , infer
  , TypeError
  , extractType
  ) where

import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.State (StateT, MonadState, runStateT, gets, modify)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.HashMap.Strict qualified as HashMap

import T.Exp (Exp, (:+)(..))
import T.Exp qualified as Exp
import T.Name (Name)
import T.Prelude
import T.SExp (sexp)
import T.SExp qualified as SExp
import T.Tmpl (Tmpl(..))


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

instance SExp.To Type where
  sexp = \case
    Unit ->
      "unit"
    Bool ->
      "bool"
    Int ->
      "int"
    Double ->
      "double"
    String ->
      "string"
    Regexp ->
      "regexp"
    Array t ->
      SExp.square [sexp t]
    Record fs ->
      SExp.curly (concatMap (\(k, v) -> [sexp k, sexp v]) (HashMap.toList fs))
    Fun args r ->
      SExp.round ["->", SExp.square (toList (map sexp args)), sexp r]
    Var n ->
      fromString ('#' : show n)

data Scheme = Forall (Set Int) Type
    deriving (Show, Eq)

forall :: [Int] -> Type -> Scheme
forall qs t =
  Forall (Set.fromList qs) t

fun1 :: Type -> Type -> Type
fun1 a1 r =
  Fun (a1 :| []) r

fun2 :: (Type, Type) -> Type -> Type
fun2 (a1, a2) r =
  Fun (a1 :| a2 : []) r

type TypedExp = Cofree Exp.ExpF (Exp.Ann, Type)

newtype InferenceT m a = InferenceT (ReaderT Γ (StateT Σ (ExceptT TypeError m)) a)
    deriving (Functor, Applicative, Monad, MonadReader Γ, MonadState Σ, MonadError TypeError)

type Γ = HashMap Name Scheme

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
    deriving (Show, Eq)

runInferenceT :: Γ -> Σ -> InferenceT m a -> m (Either TypeError (a, Σ))
runInferenceT ctx subst (InferenceT m) =
  runExceptT (runStateT (runReaderT m ctx) subst)

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

lookupCtx :: Monad m => Name -> InferenceT m Type
lookupCtx name = do
  ctx <- ask
  maybe (throwError (MissingVar name)) instantiate (HashMap.lookup name ctx)

generalize :: Γ -> Type -> Scheme
generalize ctx t = do
  let
    fvs =
      freeVarsType t
    ctxvs =
      foldMap freeVarsScheme ctx
    qs =
      Set.difference fvs ctxvs
  Forall qs t

freeVarsType :: Type -> Set Int
freeVarsType = \case
  Var n ->
    Set.singleton n
  Array t ->
    freeVarsType t
  Record r ->
    foldMap freeVarsType r
  Fun args r ->
    foldMap freeVarsType args <> freeVarsType r
  _ ->
    Set.empty

freeVarsScheme :: Scheme -> Set Int
freeVarsScheme (Forall qs t) =
  Set.difference (freeVarsType t) qs

instantiate :: Monad m => Scheme -> InferenceT m Type
instantiate (Forall vars t) = do
  fvs <- for (toList vars) $ \v -> do
    fv <- freshVar
    pure (v, fv)
  pure (minisubstitute (HashMap.fromList fvs) t)

-- | 'minisubstitute' is a variant of 'substitute' that doesn't
-- do deep substitution; this is necessary separate the namespaces
-- of quantified variables and unitification variables which is
-- useful for e.g. stdlib definitions
minisubstitute :: Subst -> Type -> Type
minisubstitute subst t =
  case t of
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
    Array arr ->
      Array (minisubstitute subst arr)
    Record r ->
      Record (map (minisubstitute subst) r)
    Fun args r ->
      Fun (map (minisubstitute subst) args) (minisubstitute subst r)
    Var n ->
      fromMaybe (Var n) (HashMap.lookup n subst)

extractType :: TypedExp -> Type
extractType ((_ann, t) :< _e) = t

unify :: Monad m => Type -> Type -> InferenceT m Type
unify t1 t2 = do
  s <- gets (.subst)
  let
    t1' =
      substitute s t1
    t2' =
      substitute s t2
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
  case substitute subst t of
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

substitute :: Subst -> Type -> Type
substitute subst t =
  case t of
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
    Array arr ->
      Array (substitute subst arr)
    Record r ->
      Record (map (substitute subst) r)
    Fun args r ->
      Fun (map (substitute subst) args) (substitute subst r)
    Var n ->
      case HashMap.lookup n subst of
        Nothing ->
          Var n
        Just nt ->
          substitute subst nt

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
    t <- case toList (map extractType ys) of
      [] ->
        freshVar
      z : zs ->
        foldM unify z zs
    pure (Array t)
  Exp.Record r -> do
    ts <- traverse inferExp r
    pure (Record (map extractType ts))

finalize :: Subst -> TypedExp -> TypedExp
finalize subst =
  map (\(ann, t) -> (ann, defaultType (substitute subst t)))

defaultType :: Type -> Type
defaultType = \case
  Var _ ->
    Unit
  Array t ->
    Array (defaultType t)
  Record r ->
    Record (map defaultType r)
  t ->
    t
