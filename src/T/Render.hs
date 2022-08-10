{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
module T.Render
  ( Env
  , render
  , envFromJson
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad ((<=<))
import           Control.Monad.Except (MonadError(..), runExcept, liftEither)
import           Control.Monad.State (MonadState(..), execStateT, modify)
import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import           Data.Foldable (for_, toList)
import qualified Data.List as List
import           Data.Scientific (floatingOrInteger)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.HashMap.Strict as HashMap
import           Prelude hiding (exp)

import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..))
import           T.Value (Value)
import qualified T.Value as Value
import           T.Embed (stdlib)


data Env = Env
  { vars   :: Value
  , result :: Builder
  }

render :: Env -> Tmpl -> Either String Lazy.Text
render env0 tmpl =
  fmap (Builder.toLazyText . result) (runExcept (execStateT (go tmpl) env0))
 where
  go = \case
    Raw str ->
      build (Builder.fromText str)
    Set name exp -> do
      value <- evalExp exp
      _ <- modifyM (insertVar name value)
      pure ()
    Let name exp tmpl0 -> do
      value <- evalExp exp
      oldEnv <- modifyM (insertVar name value)
      go tmpl0
      modify (\env -> env {vars = vars oldEnv})
    If clauses -> do
      let matchClause (exp, thenTmpl) acc = do
            value <- evalExp exp
            if truthy value then go thenTmpl else acc
      foldr matchClause (pure ()) clauses
    For name itQ exp forTmpl elseTmpl -> do
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
          throwError ("cannot iterate on: " <> Value.display value)
      case itemsQ of
        Nothing ->
          maybe (pure ()) go elseTmpl
        Just items -> do
          for_ items $ \(x, itObj) -> do
            oldEnv <-
              modifyM (maybe pure (\it -> insertVar it itObj) itQ <=< insertVar name x)
            go forTmpl
            modify (\env -> env {vars = vars oldEnv})
    Exp exp -> do
      str <- renderExp exp
      build (Builder.fromText str)
    tmpl0 :*: tmpl1 -> do
      go tmpl0
      go tmpl1
  
renderExp :: (MonadState Env m, MonadError String m) => Exp -> m Text
renderExp exp = do
  value <- evalExp exp
  case value of
    Value.Bool b ->
      pure (bool "false" "true" b)
    Value.Number n ->
      pure (fromString (either (show @Double) (show @Int) (floatingOrInteger n)))
    Value.String str ->
      pure str
    o ->
      throwError ("not renderable: " <> Value.display o)

evalExp :: (MonadState Env m, MonadError String m) => Exp -> m Value
evalExp = \case
  Lit literal ->
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
  Var name -> do
    env <- get
    case lookupVar env name of
      Nothing ->
        throwError ("not in scope: " <> show name)
      Just value -> do
        pure value
  App exp0 exp1 -> do
    fQ <- evalExp exp0
    case fQ of
      Value.Lam f -> do
        x <- evalExp exp1
        liftEither (f x)
      value ->
        throwError ("not a function: " <> Value.display value)

envFromJson :: Aeson.Value -> Env
envFromJson val = emptyEnv
  { vars = go val
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

emptyEnv :: Env
emptyEnv = Env {vars = Value.Null, result = mempty}

lookupVar :: Env -> Name -> Maybe Value
lookupVar Env {vars} (Name name) =
  go vars name <|> go stdlib name
 where
  go (Value.Object o) x = do
    HashMap.lookup x o
  go _ _ =
    Nothing

modifyM :: MonadState s m => (s -> m s) -> m s
modifyM f = do
  s <- get
  s' <- f s
  put s'
  pure s

insertVar :: MonadError String m => Name -> Value -> Env -> m Env
insertVar (Name name) value env = do
  vars' <- case vars env of
    Value.Object o
      | "_" `Text.isPrefixOf` name ->
        pure (vars env)
      | otherwise ->
        pure (Value.Object (HashMap.insert name value o))
    _ ->
      throwError ("cannot set property ." <> show name <> " to " <> Value.display value)
  pure env {vars = vars'}

build :: MonadState Env m => Builder -> m ()
build chunk =
  modify (\env -> env {result = result env <> chunk})

truthy :: Value -> Bool
truthy = \case
  Value.Null -> False
  Value.Bool False -> False
  _ -> True

loopObj :: Maybe Text -> Int -> Int -> Value
loopObj key len idx =
  Value.Object $ HashMap.fromList
    [ ("length", Value.Number (fromIntegral len))
    , ("index", Value.Number (fromIntegral idx))
    , ("first", Value.Bool (idx == 0))
    , ("last", Value.Bool (idx == len - 1))
    , ("key", maybe Value.Null Value.String key)
    ]
