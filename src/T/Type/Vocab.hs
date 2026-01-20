{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.Type.Vocab
  ( InferenceT(..)
  , runInferenceT
  , Γ
  , Σ(..)
  , Subst
  , emptyΣ
  , freshVar
  , TypeError(..)
  , TypedExp
  , Ann(..)
  , Type(..)
  , freeVarsType
  , Scheme(..)
  , freeVarsScheme
  , Constraint(..)
  , satisfies
  ) where

import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.State (StateT, MonadState, runStateT, gets, modify)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Printf (printf)
import Text.Trifecta (Span)

import T.Exp qualified as Exp
import T.Name (Name)
import T.Prelude
import T.SExp (sexp)
import T.SExp qualified as SExp


newtype InferenceT m a = InferenceT (ReaderT Γ (StateT Σ (ExceptT TypeError m)) a)
    deriving (Functor, Applicative, Monad, MonadReader Γ, MonadState Σ, MonadError TypeError)

runInferenceT :: Γ -> Σ -> InferenceT m a -> m (Either TypeError (a, Σ))
runInferenceT ctx subst (InferenceT m) =
  runExceptT (runStateT (runReaderT m ctx) subst)

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

freshVar :: Monad m => InferenceT m Type
freshVar = do
  n <- gets (.counter)
  modify (\s -> s { counter = s.counter + 1 })
  pure (Var n mempty)

data TypeError
  = MissingKey Name
  | MissingVar Name
  | NotARecord Type
  | TypeMismatch Type Type
  | ConstraintViolation Constraint Type
  | OccursCheck Int Type
    deriving (Show, Eq)

type TypedExp = Cofree (Exp.ExpF Ann) Ann

data Ann = Ann
  { spanned :: Span
  , typed   :: Type
  } deriving (Show, Eq)

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
  | Var Int (Set Constraint)
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
    Var n cs ->
      fromString (printf "#%d{%s}" n (List.intercalate ", " (map show (toList cs))))

data Scheme = Forall (Set Int) Type
    deriving (Show, Eq)

freeVarsType :: Type -> Set Int
freeVarsType = \case
  Var n _cs ->
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

data Constraint
  = Num
  | Eq
  | Display
  | Sizeable
  | Iterable
    deriving (Show, Eq, Ord)

satisfies :: Constraint -> Type -> Bool
satisfies = \case
  Num -> \case
    Int -> True
    Double -> True
    Var _n cs -> Num `Set.member` cs
    _ -> False
  Eq -> \case
    Unit -> True
    Bool -> True
    Int -> True
    Double -> True
    String -> True
    Regexp -> True
    Array t ->
      satisfies Eq t
    Record fs ->
      all (satisfies Eq) fs
    Var _n cs -> Eq `Set.member` cs
    _ -> False
  Display -> \case
    Unit -> True
    Bool -> True
    Int -> True
    Double -> True
    String -> True
    Array t ->
      satisfies Display t
    Record fs ->
      all (satisfies Display) fs
    Var _n cs -> Display `Set.member` cs
    _ -> False
  Sizeable -> \case
    String -> True
    Array _ -> True
    Record _ -> True
    Var _n cs -> Sizeable `Set.member` cs
    _ -> False
  Iterable -> \case
    Array _ -> True
    Record _ -> True
    Var _n cs -> Iterable `Set.member` cs
    _ -> False
