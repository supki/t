{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module T.Render
  ( Env
  , render
  , envFromJson
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.Except (MonadError(..), runExcept, liftEither)
import           Control.Monad.State (MonadState(..), evalStateT)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import           Data.Foldable (toList)
import qualified Data.List as List
import           Data.Scientific (floatingOrInteger)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Traversable (for)
import qualified Data.HashMap.Strict as HashMap
import           Prelude hiding (exp)

import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..))
import           T.Value (Value)
import qualified T.Value as Value
import           T.Embed (stdlib)


newtype Env = Env { unEnv :: Value }

render :: Env -> Tmpl -> Either String Lazy.Text
render env0 tmpl =
  fmap Builder.toLazyText (runExcept (evalStateT (go tmpl) env0))
 where
  go = \case
    Raw str ->
      pure (Builder.fromText str)
    Set name exp -> do
      value <- evalExp exp
      modifyM (insertVar name value)
      pure ""
    If clauses -> do
      let matchClause (exp, thenTmpl) acc = do
            value <- evalExp exp
            if ifTrue value then go thenTmpl else acc
      foldr matchClause (pure "") clauses
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
          maybe (pure "") go elseTmpl
        Just items -> do
          rs <- for items $ \(x, itObj) -> do
            env <- get
            env' <- insertVar name x env
            env'' <- case itQ of
              Nothing ->
                pure env'
              Just it ->
                insertVar it itObj env'
            lift (evalStateT (go forTmpl) env'')
          pure (mconcat rs)
    Exp exp -> do
      str <- renderExp exp
      pure (Builder.fromText str)
    tmpl0 :*: tmpl1 -> do
      b0 <- go tmpl0
      b1 <- go tmpl1
      pure (b0 <> b1)

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = do 
  s <- get
  s' <- f s
  put s'
  
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
envFromJson =
  Env . go
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

lookupVar :: Env -> Name -> Maybe Value
lookupVar (Env env) (Name name) =
  go env (toList name) <|> go stdlib (toList name)
 where
  go o [] =
    pure o
  go (Value.Object o) (x : xs) = do
    o' <- HashMap.lookup x o
    go o' xs
  go _ _ =
    Nothing

insertVar :: MonadError String m => Name -> Value -> Env -> m Env
insertVar (Name name) value (Env env) =
  fmap Env (go env (toList name))
 where
  go _ [] =
    pure value
  go (Value.Object o) (x : xs) = do
    case HashMap.lookup x o of
      Nothing -> do
        v <- go Value.Null xs
        pure (Value.Object (HashMap.insert x v o))
      Just o' -> do
        v <- go o' xs
        pure (Value.Object (HashMap.insert x v o))
  go o (x : _) =
    throwError ("cannot set property ." <> show x <> " to " <> Value.display o)

ifTrue :: Value -> Bool
ifTrue = \case
  Value.Null -> False
  Value.Bool False -> False
  _ -> True

loopObj :: Maybe Text -> Int -> Int -> Value
loopObj key length idx =
  Value.Object $ HashMap.fromList
    [ ("length", Value.Number (fromIntegral length))
    , ("index", Value.Number (fromIntegral idx))
    , ("first", Value.Bool (idx == 0))
    , ("last", Value.Bool (idx == length - 1))
    , ("key", maybe Value.Null Value.String key)
    ]
