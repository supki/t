{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Data.HashMap.Strict (HashMap)
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

import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..), (:<)(..), Ann)
import           T.Exp.Ann (emptyAnn)
import           T.Error (Error(..))
import           T.Stdlib (stdlib)
import           T.Value (Value)
import qualified T.Value as Value


data Env = Env
  { vars   :: HashMap Name (Ann, Value)
  , result :: Builder
  }

render :: Env -> Tmpl -> Either Error Lazy.Text
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
          throwError (GenericError ("cannot iterate on: " <> Value.display value))
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
      throwError (GenericError ("not renderable: " <> Value.display o))

evalExp :: (MonadState Env m, MonadError Error m) => Exp -> m Value
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
    lookupVar env name
  App exp0 exp1 -> do
    fQ <- evalExp exp0
    case fQ of
      Value.Lam f -> do
        x <- evalExp exp1
        liftEither (f x)
      value ->
        throwError (GenericError ("not a function: " <> Value.display value))

envFromJson :: Aeson.Value -> Maybe Env
envFromJson (Aeson.Object val) =
  Just emptyEnv
    { vars = fmap (\x -> (emptyAnn, go x)) (HashMap.mapKeys Name val)
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
envFromJson _ =
  Nothing

emptyEnv :: Env
emptyEnv = Env {vars = mempty, result = mempty}

lookupVar :: MonadError Error m => Env -> Ann :< Name -> m Value
lookupVar Env {vars} aname@(_ :< name) =
  case fmap snd (HashMap.lookup name vars) <|> HashMap.lookup name stdlib of
    Nothing ->
      throwError (NotInScope aname)
    Just value ->
      pure value

modifyM :: MonadState s m => (s -> m s) -> m s
modifyM f = do
  s <- get
  s' <- f s
  put s'
  pure s

insertVar :: MonadError Error m => Ann :< Name -> Value -> Env -> m Env
insertVar (_ :< Name (Text.uncons -> Just ('_', _rest))) _ env =
  pure env
insertVar (ann :< name) value env =
  case HashMap.lookup name (vars env) of
    Nothing ->
      pure env {vars = HashMap.insert name (ann, value) (vars env)}
    Just (oldAnn, _) ->
      throwError (ShadowedBy (oldAnn :< name) (ann :< name))

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
