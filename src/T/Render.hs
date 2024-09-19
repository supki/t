{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module T.Render
  ( Env(..)
  , Scope(..)
  , render
  , exec
  ) where

import Control.Monad.Except (MonadError(..), ExceptT, runExcept, liftEither)
import Control.Monad.State (MonadState(..), StateT, execStateT, evalStateT, modify)
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Foldable (asum, for_, toList)
import Data.HashMap.Strict (HashMap)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (floatingOrInteger)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Data.HashMap.Strict qualified as HashMap
import Prelude hiding (exp, lookup)

import T.Exp (Cofree(..), Exp, ExpF(..), Literal(..), (:+)(..), Ann)
import T.Exp.Ann (emptyAnn)
import T.Error (Error(..), Warning(..))
import T.Name (Name(..))
import T.Stdlib (Stdlib)
import T.Stdlib qualified as Stdlib
import T.Value (Value)
import T.Value qualified as Value
import T.Tmpl (Tmpl((:*:)))
import T.Tmpl qualified as Tmpl


data Env = Env
  { stdlib   :: HashMap Name Value
  , scope    :: HashMap Name (Ann, Value)
  , result   :: Builder
  , warnings :: Set (Ann, Warning)
  }

newtype Scope = Scope (HashMap Name Value)
    deriving (Semigroup, Monoid)

-- | Convert a template to text using the provided environment.
render :: (Stdlib, Scope) -> Tmpl -> Either Error ([Warning], Lazy.Text)
render (uncurry mkEnv -> env0) tmpl =
  fmap fromEnv (run env0 tmpl)
 where
  fromEnv env =
    ( map snd (Set.toAscList env.warnings)
    , Builder.toLazyText env.result
    )

-- | Collect the environment-changing side-effects.
exec :: (Stdlib, Scope) -> Tmpl -> Either Error ([Warning], Scope)
exec (uncurry mkEnv -> env0) tmpl =
  fmap fromEnv (run env0 tmpl)
 where
  fromEnv env =
    ( map snd (Set.toAscList env.warnings)
    , Scope (fmap snd env.scope)
    )

mkEnv :: Stdlib -> Scope -> Env
mkEnv stdlib (Scope vars) = Env
  { stdlib = Stdlib.bindings stdlib
  , scope = fmap (\x -> (emptyAnn, x)) vars
  , result = mempty
  , warnings = mempty
  }

run :: Env -> Tmpl -> Either Error Env
run env0 tmpl =
  runExcept (execStateT (go tmpl) env0)
 where
  go :: Monad m => Tmpl -> StateT Env (ExceptT Error m) ()
  go = \case
    Tmpl.Raw str ->
      build (Builder.fromText str)
    Tmpl.Comment _str ->
      pure ()
    Tmpl.Set assignments -> do
      for_ assignments $ \(Tmpl.Assign name exp) -> do
        value <- evalExp exp
        insertVar name value
    Tmpl.Let assignments tmpl0 -> do
      oldEnv <- get
      for_ assignments $ \(Tmpl.Assign name exp) -> do
        value <- evalExp exp
        insertVar name value
      go tmpl0
      modify (\env -> env {scope = scope oldEnv})
    Tmpl.If clauses -> do
      let matchClause (exp, thenTmpl) acc = do
            value <- evalExp exp
            if Value.truthy value then go thenTmpl else acc
      foldr matchClause (pure ()) clauses
    Tmpl.For name itQ exp forTmpl elseTmpl -> do
      value <- evalExp exp
      itemsQ <- case value of
        Value.Array arr -> do
          let xs =
                zipWith
                  (\i x -> (x, loopObj Nothing (length arr) i))
                  [0..]
                  (toList arr)
          pure (bool (Just xs) Nothing (null xs))
        Value.Object o -> do
          -- When iterating on objects, we sort the keys to get a
          -- predictable order of elements.
          let xs =
                zipWith
                  (\i (k, x) -> (x, loopObj (pure k) (length o) i))
                  [0..]
                  (List.sortOn
                    fst
                    (HashMap.toList o))
          pure (bool (Just xs) Nothing (null xs))
        _ ->
          throwError (NotIterable exp (Value.display value))
      case itemsQ of
        Nothing ->
          maybe (pure ()) go elseTmpl
        Just items -> do
          for_ items $ \(x, itObj) -> do
            oldEnv <- get
            insertVar name x
            for_ itQ (\it -> insertVar it itObj)
            go forTmpl
            modify (\env -> env {scope = scope oldEnv})
    Tmpl.Exp exp -> do
      str <- renderExp exp
      build (Builder.fromText str)
    tmpl0 :*: tmpl1 -> do
      go tmpl0
      go tmpl1

renderExp :: (MonadState Env m, MonadError Error m) => Exp -> m Text
renderExp exp = do
  value <- evalExp exp
  case value of
    Value.Null ->
      pure ""
    Value.Bool b ->
      pure (bool "false" "true" b)
    Value.Number n ->
      pure (fromString (either (show @Double) (show @Int) (floatingOrInteger n)))
    Value.String str ->
      pure str
    o ->
      throwError (NotRenderable exp (Value.display o))

evalExp :: (MonadState Env m, MonadError Error m) => Exp -> m Value
evalExp = \case
  _ :< Lit literal ->
    case literal of
      Null ->
        pure Value.Null
      Bool b ->
        pure (Value.Bool b)
      Number n ->
        pure (Value.Number n)
      String str ->
        pure (Value.String str)
      Regexp str ->
        pure (Value.Regexp str)
      Array xs -> do
        ys <- traverse evalExp xs
        pure (Value.Array ys)
      Object xs -> do
        ys <- traverse evalExp xs
        pure (Value.Object ys)
  _ :< If p t f -> do
    pv <- evalExp p
    evalExp (bool f t (Value.truthy pv))
  _ :< Var name -> do
    env <- get
    lookupVar env name
  _ :< App "defined?" (exp1 :| []) -> do
    -- this is a very basic form of try-catch;
    -- it gives no control to the user over what kind of exceptions
    -- to catch: it catches all of them indiscriminately.
    env <- get
    let subEval exp = runExcept (evalStateT (evalExp exp) env)
    pure (Value.Bool (isRight (subEval exp1)))
  ann :< App name args -> do
    f <- evalExp (ann :< Var name)
    evalApp name f (toList args)

evalApp
  :: (MonadState Env m, MonadError Error m)
  => Ann :+ Name
  -> Value
  -> [Exp]
  -> m Value
evalApp name =
  go
 where
  -- either we have to arguments left, so we return whatever we have
  go val [] =
    pure val
  -- or we have a function to apply to the next argument
  go (Value.Lam f) (exp0@(ann :< _) : exps) = do
    x <- evalExp exp0
    g <- liftEither (f (ann :+ x))
    go g exps
  -- in every other case something went wrong :-(
  go val _ =
    throwError (NotAFunction name (Value.display val))

lookupVar :: MonadError Error m => Env -> Ann :+ Name -> m Value
lookupVar env (ann :+ name) =
  case lookup env name of
    Nothing ->
      throwError (NotInScope (ann :+ name))
    Just (_, value) ->
      pure value

insertVar :: MonadState Env m => Ann :+ Name -> Value -> m ()
insertVar (_ :+ Name (Text.uncons -> Just ('_', _rest))) _ =
  pure ()
insertVar (ann :+ name) value = do
  env <- get
  for_ (lookup env name) $ \(ann', _value) ->
    warn ann (ShadowedBy ((ann', ann) :+ name))
  modify $ \env' -> env'
    { scope = HashMap.insert name (ann, value) (scope env')
    }

lookup :: Env -> Name -> Maybe (Ann, Value)
lookup env name =
  asum
    [ HashMap.lookup name env.scope
    , do
      value <- HashMap.lookup name env.stdlib
      pure (emptyAnn, value)
    ]

warn :: MonadState Env m => Ann -> Warning -> m ()
warn ann warning =
  modify (\s -> s {warnings = Set.insert (ann, warning) (warnings s)})

build :: MonadState Env m => Builder -> m ()
build chunk =
  modify (\env -> env {result = result env <> chunk})

loopObj :: Maybe Text -> Int -> Int -> Value
loopObj key len idx =
  Value.Object $ HashMap.fromList
    [ ("length", Value.Number (fromIntegral len))
    , ("index", Value.Number (fromIntegral idx))
    , ("first", Value.Bool (idx == 0))
    , ("last", Value.Bool (idx == len - 1))
    , ("key", maybe Value.Null Value.String key)
    ]
