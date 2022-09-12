{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module T.Render
  ( Env
  , mkDefEnv
  , mkEnv
  , render
  ) where

import           Control.Monad.Except (MonadError(..), runExcept, liftEither)
import           Control.Monad.State (MonadState(..), execStateT, evalStateT, modify)
import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import           Data.Either (isRight)
import           Data.Foldable (asum, for_, toList)
import           Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Scientific (floatingOrInteger)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.HashMap.Strict as HashMap
import           Prelude hiding (exp, lookup)

import           T.Exp (Cofree(..), Exp, ExpF(..), Literal(..), Name(..), (:+)(..), Ann)
import           T.Exp.Ann (emptyAnn)
import           T.Error (Error(..), Warning(..))
import qualified T.Stdlib as Stdlib
import           T.Value (Value)
import qualified T.Value as Value
import           T.Tmpl (Tmpl((:*:)))
import qualified T.Tmpl as Tmpl


data Env = Env
  { stdlib   :: HashMap Name Value
  , scope    :: HashMap Name (Ann, Value)
  , result   :: Builder
  , warnings :: Set (Ann, Warning)
  }

mkDefEnv :: HashMap Name Aeson.Value -> Env
mkDefEnv =
  mkEnv Stdlib.def

mkEnv :: HashMap Name Value -> HashMap Name Aeson.Value -> Env
mkEnv stdlibExt vars = Env
  { stdlib = HashMap.union Stdlib.def stdlibExt
  , scope = fmap (\x -> (emptyAnn, go x)) vars
  , result = mempty
  , warnings = mempty
  }
 where
  go = \case
    Aeson.Null ->
      Value.Null
    Aeson.Bool b ->
      Value.Bool b
    Aeson.Number n ->
      Value.Number n
    Aeson.String str ->
      Value.String str
    Aeson.Array xs ->
      Value.Array (fmap go xs)
    Aeson.Object xs ->
      Value.Object (fmap go xs)

render :: Env -> Tmpl -> Either Error ([Warning], Lazy.Text)
render env0 tmpl =
  fmap fromEnv (runExcept (execStateT (go tmpl) env0))
 where
  fromEnv Env {result, warnings} =
    ( map snd (Set.toAscList warnings)
    , Builder.toLazyText result
    )

  go = \case
    Tmpl.Raw str ->
      build (Builder.fromText str)
    Tmpl.Set name exp -> do
      value <- evalExp exp
      insertVar name value
      pure ()
    Tmpl.Let name exp tmpl0 -> do
      value <- evalExp exp
      oldEnv <- get
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
lookup Env {stdlib, scope} name =
  asum
    [ HashMap.lookup name scope
    , do
      value <- HashMap.lookup name stdlib
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
