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
import Data.Vector ((!?), (//))

import T.Exp (Exp, ExpF(..), Literal(..), (:+)(..), Ann)
import T.Exp.Ann (emptyAnn)
import T.Error (Error(..), Warning(..))
import T.Name (Name(..))
import T.Name qualified as Name
import T.Prelude
import T.Render.Rendering (Rendering(..))
import T.Render.Rendering qualified as Rendering
import T.SExp (sexp)
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
  Tmpl.Set assignments ->
    traverse_ evalAssign assignments
  Tmpl.Let assignments tmpl0 -> do
    r0 <- get
    traverse_ evalAssign assignments
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
        throwError (TagMismatch exp (Type.Var 0 (Set.fromList [Type.Iterable])) (Value.typeOf value) (sexp value))
    case itemsQ of
      Nothing ->
        maybe (pure ()) renderTmpl elseTmpl
      Just items -> do
        for_ items $ \(x, itRecord) -> do
          r0 <- get
          insertVar (topLevel name) x
          for_ itQ (\it -> insertVar (topLevel it) itRecord)
          renderTmpl forTmpl
          modify (\r -> r {Rendering.scope = r0.scope})
  Tmpl.Exp exp -> do
    str <- renderExp exp
    build (Builder.fromText str)
  Tmpl.Cat tmpls ->
    traverse_ renderTmpl tmpls

evalAssign :: (Ctx m, MonadError Error m) => Tmpl.Assign -> m ()
evalAssign assign = do
  (_lv, path) <- evalLValue assign.lvalue
  rv <- evalExp assign.rvalue
  insertVar path rv

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
      throwError (TagMismatch exp (Type.Var 0 (Set.fromList [Type.Render])) (Value.typeOf value) (sexp value))

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
    case xs !? idx of
      Nothing ->
        throwError (OutOfBounds expIdx (sexp xs) (sexp idx))
      Just x ->
        pure x
  _ :< Key exp (_ :+ key) -> do
    r <- enforceRecord exp
    case HashMap.lookup key r of
      Nothing ->
        throwError (MissingField exp (sexp r) (sexp key))
      Just x ->
        pure x

enforceInt :: (Ctx m, MonadError Error m) => Exp -> m Int
enforceInt exp = do
  v <- evalExp exp
  case v of
    Value.Int xs ->
      pure xs
    _ ->
      throwError (TagMismatch exp Type.Int (Value.typeOf v) (sexp v))

enforceArray :: (Ctx m, MonadError Error m) => Exp -> m (Vector Value)
enforceArray exp = do
  v <- evalExp exp
  case v of
    Value.Array xs ->
      pure xs
    _ ->
      throwError (TagMismatch exp (Type.Array (Type.tyVar 0)) (Value.typeOf v) (sexp v))

enforceRecord :: (Ctx m, MonadError Error m) => Exp -> m (HashMap Name Value)
enforceRecord exp = do
  v <- evalExp exp
  case v of
    Value.Record r ->
      pure r
    _ ->
      throwError (TagMismatch exp (Type.Record mempty) (Value.typeOf v) (sexp v))

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
    throwError (TagMismatch (ann0 :< Var name) (Type.tyVar 0 `Type.fun1` Type.tyVar 1) (Value.typeOf v) (sexp v))

data Path = Path
  { var     :: Ann :+ Name
  , lookups :: [Lookup]
  } deriving (Show, Eq)

data Lookup
  = K (Ann :+ Name)
  | I (Ann :+ Int)
    deriving (Show, Eq)

topLevel :: Ann :+ Name -> Path
topLevel var = Path {var, lookups = []}

evalLValue :: (Ctx m, MonadError Error m) => Exp -> m (Either (Ann :+ Name) Value, Path)
evalLValue =
  map (second (\p -> p {lookups = reverse p.lookups})) . go
 where
  go exp =
    case exp of
      _ :< Lit _literal ->
        throwError (NotLValue exp)
      _ :< If _p _t _f ->
        throwError (NotLValue exp)
      _ :< App _name _args -> do
        throwError (NotLValue exp)
      _ :< Var name -> do
        v <- map Right (lookupVar name) `catchError` \_ -> pure (Left name)
        pure (v, Path {var = name, lookups = []})
      _ :< Idx exp0 expIdx@(idxAnn :< _) -> do
        (xs0, path0) <- go exp0
        idx <- enforceInt expIdx
        case xs0 of
          Right (Value.Array xs) -> do
            let
              path =
                path0 {lookups = I (idxAnn :+ idx) : path0.lookups}
            pure $ case xs !? idx of
              Nothing -> do
                let
                  key =
                    idxAnn :+ Name (fromString ("[" <> show idx <> "]"))
                (Left key, path)
              Just v ->
                (Right v, path)
          Right v ->
            throwError (TagMismatch exp (Type.Array (Type.tyVar 0)) (Value.typeOf v) (sexp v))
          Left name ->
            throwError (NotInScope name)
      _ :< Key exp0 key@(_ :+ key0) -> do
        (r0, path0) <- go exp0
        case r0 of
          Right (Value.Record r) -> do
            let
              path =
                path0 {lookups = K key : path0.lookups}
            pure $ case HashMap.lookup (fromString (Name.toString key0)) r of
              Nothing ->
                (Left key, path)
              Just v ->
                (Right v, path)
          Right v ->
            throwError (TagMismatch exp (Type.Record mempty) (Value.typeOf v) (sexp v))
          Left name ->
            throwError (NotInScope name)

lookupVar :: (Ctx m, MonadError Error m) => Ann :+ Name -> m Value
lookupVar (ann :+ name) = do
  valueQ <- lookup name
  case valueQ of
    Nothing ->
      throwError (NotInScope (ann :+ name))
    Just (_, value) ->
      pure value

insertVar :: (Ctx m, MonadError Error m) => Path -> Value -> m ()
-- assignments to variables which name starts with '_' are ignored
-- if there are no further lookups; if there are further lookups,
-- the assignment will likely end in a `NotInScope` error, since
-- there aren't any variables with the name starting with '_' in the
-- default scope.
insertVar Path {var = (_ :+ Name (Text.uncons -> Just ('_', _name))), lookups = []} _v =
  pure ()
insertVar Path {var = (ann :+ name), lookups} v = do
  -- first, we need to check if the variable is in scope
  value0Q <- lookup name
  value <- case value0Q of
    -- if it isn't, we run `lookups` on `Null`. this will only succeed
    -- if `lookups` is the empty list, i.e. only simple assignments will
    -- work, e.g. {% set foo = 4 %}
    Nothing ->
      go Value.Null lookups
    Just (ann', value0) -> do
      -- if it is, but `lookups` is empty, we are shadowing an existing
      -- variable and need to issue a warning
      when (List.null lookups) $
        warn ann (ShadowedBy ((ann', ann) :+ name))
      -- and then run the lookups
      go value0 lookups
  -- `go` returns a value that we will assign to the variable `name`
  --  in the current scope.
  modify $ \env' -> env'
    { Rendering.scope = HashMap.insert name (ann, value) env'.scope
    }
 where
  -- for each element of the path we ensure the value is of the correct form
  go (Value.Record r) (K (ann0 :+ key) : path) =
    case HashMap.lookup (fromString (Name.toString key)) r of
      Just v1 -> do
        -- and recurse deeper, until the path is empty.
        v2 <- go v1 path
        pure (Value.Record (HashMap.insert (fromString (Name.toString key)) v2 r))
      Nothing ->
        -- arrays are fixed size, but records need a bit of special treatment
        -- if we want to be able to add properties that haven't been defined yet.
        case path of
          [] ->
            pure (Value.Record (HashMap.insert (fromString (Name.toString key)) v r))
          _ ->
            throwError (MissingField (ann0 :< Lit Null) (sexp r) (sexp key))
  go v0 (K (ann0 :+ _key) : _path) =
    throwError (TagMismatch (ann0 :< Lit Null) (Type.Record mempty) (Value.typeOf v0) (sexp v0))
  go (Value.Array xs) (I (ann0 :+ idx) : path) =
    -- this is pretty similar to records except the lack of the aforementioned special
    -- treatment.
    case xs !? idx of
      Just v1 -> do
        v2 <- go v1 path
        pure (Value.Array (xs // [(idx, v2)]))
      Nothing ->
        throwError (OutOfBounds (ann0 :< Lit Null) (sexp xs) (sexp idx))
  go v0 (I (ann0 :+ _idx) : _path) =
    throwError (TagMismatch (ann0 :< Lit Null) (Type.Record mempty) (Value.typeOf v0) (sexp v0))
  go _v0 [] =
    pure v

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

loopRecord :: Maybe Name -> Int -> Int -> Value
loopRecord key len idx =
  Value.Record $ HashMap.fromList
    [ ("length", Value.Int len)
    , ("index", Value.Int idx)
    , ("first", Value.Bool (idx == 0))
    , ("last", Value.Bool (idx == len - 1))
    , ("key", maybe Value.Null (Value.String . Name.toText) key)
    ]
