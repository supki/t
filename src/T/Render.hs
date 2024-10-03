{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module T.Render
  ( Rendered(..)
  , Scope(..)
  , render
  ) where

import Control.Monad.Except (MonadError(..), runExcept, liftEither)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.State (MonadState(..), execStateT, evalStateT, modify)
import Data.Either (isRight)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Data.HashMap.Strict qualified as HashMap
import Data.Vector ((!?))

import T.Exp (Cofree(..), Exp, ExpF(..), Literal(..), (:+)(..), Ann)
import T.Exp.Ann (emptyAnn)
import T.Error (Error(..), Warning(..))
import T.Name (Name(..))
import T.Prelude
import T.Render.Rendering (Rendering(..))
import T.Render.Rendering qualified as Rendering
import T.Stdlib (Stdlib)
import T.Stdlib qualified as Stdlib
import T.Value (Value)
import T.Value qualified as Value
import T.Tmpl (Tmpl)
import T.Tmpl qualified as Tmpl
import T.Type qualified as Type


data Rendered = Rendered
  { scope    :: Scope
  , result   :: Lazy.Text
  , warnings :: [Warning]
  }

data Env = Env
  { stdlib :: HashMap Name Value
  }

-- | Context common for most rendering functions.
type Ctx m = (MonadReader Env m, MonadState Rendering m)

newtype Scope = Scope (HashMap Name Value)
    deriving (Semigroup, Monoid)

-- | Convert a template to text using the provided environment.
render :: (Stdlib, Scope) -> Tmpl -> Either Error Rendered
render (stdlib, scope) tmpl =
  map rendered (run (mkEnv stdlib) (mkRendering scope) tmpl)

rendered :: Rendering -> Rendered
rendered r = Rendered
  { scope =
      Scope (map (\(_, w) -> w) r.scope)
  , result =
      Builder.toLazyText r.result
  , warnings =
      map (\(_, w) -> w) (Set.toAscList r.warnings)
  }

mkEnv :: Stdlib -> Env
mkEnv stdlib = Env
  { stdlib = Stdlib.bindings stdlib
  }

mkRendering :: Scope -> Rendering
mkRendering (Scope vars) = Rendering
  { scope = map (\x -> (emptyAnn, x)) vars
  , result = mempty
  , warnings = mempty
  }

run :: Env -> Rendering -> Tmpl -> Either Error Rendering
run env0 r0 tmpl =
  runExcept (execStateT (runReaderT (renderTmpl tmpl) env0) r0)

renderTmpl :: (Ctx m, MonadError Error m) => Tmpl -> m ()
renderTmpl = \case
  Tmpl.Raw str ->
    build (Builder.fromText str)
  Tmpl.Comment _str ->
    pure ()
  Tmpl.Set assignments -> do
    for_ assignments $ \(Tmpl.Assign name exp) -> do
      value <- evalExp exp
      insertVar name value
  Tmpl.Let assignments tmpl0 -> do
    r0 <- get
    for_ assignments $ \(Tmpl.Assign name exp) -> do
      value <- evalExp exp
      insertVar name value
    renderTmpl tmpl0
    modify (\r -> r {Rendering.scope = r0.scope})
  Tmpl.If clauses -> do
    let matchClause (exp, thenTmpl) acc = do
          value <- evalExp exp
          if Value.truthy value then renderTmpl thenTmpl else acc
    foldr matchClause (pure ()) clauses
  Tmpl.For name itQ exp forTmpl elseTmpl -> do
    value <- evalExp exp
    itemsQ <- case value of
      Value.Array arr -> do
        let xs =
              zipWith
                (\i x -> (x, loopRecord Nothing (List.length arr) i))
                [0..]
                (toList arr)
        pure (bool (Just xs) Nothing (List.null xs))
      Value.Record o -> do
        -- When iterating on records, we sort the keys to get a
        -- predictable order of elements.
        let xs =
              zipWith
                (\i (k, x) -> (x, loopRecord (pure k) (List.length o) i))
                [0..]
                (List.sortOn
                  (\(k, _) -> k)
                  (HashMap.toList o))
        pure (bool (Just xs) Nothing (List.null xs))
      _ ->
        throwError (TypeError exp Type.Iterable (Value.typeOf value) (Value.display value))
    case itemsQ of
      Nothing ->
        maybe (pure ()) renderTmpl elseTmpl
      Just items -> do
        for_ items $ \(x, itRecord) -> do
          r0 <- get
          insertVar name x
          for_ itQ (\it -> insertVar it itRecord)
          renderTmpl forTmpl
          modify (\r -> r {Rendering.scope = r0.scope})
  Tmpl.Exp exp -> do
    str <- renderExp exp
    build (Builder.fromText str)
  Tmpl.Cat tmpls ->
    traverse_ renderTmpl tmpls

renderExp :: (Ctx m, MonadError Error m) => Exp -> m Text
renderExp exp = do
  value <- evalExp exp
  case value of
    Value.Null ->
      pure ""
    Value.Bool b ->
      pure (bool "false" "true" b)
    Value.Int n ->
      pure (fromString (show n))
    Value.Double n ->
      pure (fromString (show n))
    Value.String str ->
      pure str
    _ ->
      throwError (TypeError exp Type.Renderable (Value.typeOf value) (Value.display value))

evalExp :: (Ctx m, MonadError Error m) => Exp -> m Value
evalExp = \case
  _ :< Lit literal ->
    case literal of
      Null ->
        pure Value.Null
      Bool b ->
        pure (Value.Bool b)
      Int n ->
        pure (Value.Int n)
      Double n ->
        pure (Value.Double n)
      String str ->
        pure (Value.String str)
      Regexp str ->
        pure (Value.Regexp str)
      Array xs -> do
        ys <- traverse evalExp xs
        pure (Value.Array ys)
      Record xs -> do
        ys <- traverse evalExp xs
        pure (Value.Record ys)
  _ :< If p t f -> do
    pv <- evalExp p
    evalExp (bool f t (Value.truthy pv))
  _ :< Var name -> do
    lookupVar name
  _ :< App "defined?" (exp1 :| []) -> do
    -- this is a very basic form of try-catch;
    -- it gives no control to the user over what kind of exceptions
    -- to catch: it catches all of them indiscriminately.
    env <- ask
    r <- get
    let
      subEval exp =
        runExcept (evalStateT (runReaderT (evalExp exp) env) r)
    pure (Value.Bool (isRight (subEval exp1)))
  ann :< App name args -> do
    f <- evalExp (ann :< Var name)
    evalApp name f (toList args)
  _ :< Idx exp expIdx -> do
    xs <- enforceArray exp
    idx <- enforceInt expIdx
    case xs !? fromIntegral idx of
      Nothing ->
        throwError (OutOfBounds expIdx (Value.display (Value.Array xs)) (Value.display (Value.Int idx)))
      Just x ->
        pure x
  _ :< Key exp (_ :+ Name key) -> do
    r <- enforceRecord exp
    case HashMap.lookup key r of
      Nothing ->
        throwError (MissingProperty exp (Value.display (Value.Record r)) (Value.display (Value.String key)))
      Just x ->
        pure x

enforceInt :: (Ctx m, MonadError Error m) => Exp -> m Int64
enforceInt exp = do
  v <- evalExp exp
  case v of
    Value.Int xs ->
      pure xs
    _ ->
      throwError (TypeError exp Type.Int (Value.typeOf v) (Value.display v))

enforceArray :: (Ctx m, MonadError Error m) => Exp -> m (Vector Value)
enforceArray exp = do
  v <- evalExp exp
  case v of
    Value.Array xs ->
      pure xs
    _ ->
      throwError (TypeError exp Type.Array (Value.typeOf v) (Value.display v))

enforceRecord :: (Ctx m, MonadError Error m) => Exp -> m (HashMap Text Value)
enforceRecord exp = do
  v <- evalExp exp
  case v of
    Value.Record r ->
      pure r
    _ ->
      throwError (TypeError exp Type.Record (Value.typeOf v) (Value.display v))

evalApp
  :: (Ctx m, MonadError Error m)
  => Ann :+ Name
  -> Value
  -> [Exp]
  -> m Value
evalApp name@(ann0 :+ _) =
  go
 where
  -- either we have to arguments left, so we return whatever we have
  go v [] =
    pure v
  -- or we have a function to apply to the next argument
  go (Value.Lam f) (exp0@(ann :< _) : exps) = do
    x <- evalExp exp0
    g <- liftEither (f (ann :+ x))
    go g exps
  -- in every other case something went wrong :-(
  go v _ =
    throwError (TypeError (ann0 :< Var name) Type.Fun (Value.typeOf v) (Value.display v))

lookupVar :: (Ctx m, MonadError Error m) => Ann :+ Name -> m Value
lookupVar (ann :+ name) = do
  valueQ <- lookup name
  case valueQ of
    Nothing ->
      throwError (NotInScope (ann :+ name))
    Just (_, value) ->
      pure value

insertVar :: Ctx m => Ann :+ Name -> Value -> m ()
insertVar (_ :+ Name (Text.uncons -> Just ('_', _rest))) _ =
  pure ()
insertVar (ann :+ name) value = do
  valueQ <- lookup name
  for_ valueQ $ \(ann', _value) ->
    warn ann (ShadowedBy ((ann', ann) :+ name))
  modify $ \env' -> env'
    { Rendering.scope = HashMap.insert name (ann, value) env'.scope
    }

lookup :: Ctx m => Name -> m (Maybe (Ann, Value))
lookup name = do
  env <- ask
  r <- get
  pure $ asum
    [ HashMap.lookup name r.scope
    , do
      value <- HashMap.lookup name env.stdlib
      pure (emptyAnn, value)
    ]

warn :: MonadState Rendering m => Ann -> Warning -> m ()
warn ann warning =
  modify (\s -> s {Rendering.warnings = Set.insert (ann, warning) s.warnings})

build :: MonadState Rendering m => Builder -> m ()
build chunk =
  modify (\env -> env {Rendering.result = env.result <> chunk})

loopRecord :: Maybe Text -> Int -> Int -> Value
loopRecord key len idx =
  Value.Record $ HashMap.fromList
    [ ("length", Value.Int (fromIntegral len))
    , ("index", Value.Int (fromIntegral idx))
    , ("first", Value.Bool (idx == 0))
    , ("last", Value.Bool (idx == len - 1))
    , ("key", maybe Value.Null Value.String key)
    ]
