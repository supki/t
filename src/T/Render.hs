{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module T.Render
  ( Env(..)
  , render
  ) where

import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import           Data.Foldable (toList)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.HashMap.Strict as HashMap
import           Prelude hiding (exp)

import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..))
import           T.Value (Value)
import qualified T.Value as Value


newtype Env = Env { unEnv :: Aeson.Value }
    deriving (Show, Eq)

render :: Env -> Tmpl -> Either String Lazy.Text
render env = fmap Builder.toLazyText . go
 where
  go = \case
    Raw str ->
      pure (Builder.fromText str)
    Exp exp -> do
      str <- renderExp env exp
      pure (Builder.fromText str)
    tmpl0 :*: tmpl1 -> do
      b0 <- go tmpl0
      b1 <- go tmpl1
      pure (b0 <> b1)
  
renderExp :: Env -> Exp -> Either String Text
renderExp env exp = do
  value <- evalExp env exp
  case value of
    (Value.Bool b) ->
      pure (bool "false" "true" b)
    (Value.Number n) ->
      pure (fromString (show n))
    (Value.String str) ->
      pure str
    o ->
      Left ("not renderable: " <> show o)

evalExp :: Env -> Exp -> Either String Value
evalExp env = \case
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
        ys <- traverse (evalExp env) xs
        pure (Value.Array ys)
  Var name ->
    case lookupVar env name of
      Nothing ->
        Left ("not in scope: " <> show name)
      Just json -> do
        value <- fromJson json
        evalExp env value
 where
  fromJson :: Aeson.Value -> Either String Exp
  fromJson = \case
    Aeson.Null ->
      pure (Lit Null)
    Aeson.Bool b ->
      pure (Lit (Bool b))
    Aeson.Number n ->
      pure (Lit (Number n))
    Aeson.String str ->
      pure (Lit (String str))
    Aeson.Array xs -> do
      ys <- traverse fromJson xs
      pure (Lit (Array (toList ys)))
    o ->
      Left ("unsupported json: " <> show o)

lookupVar :: Env -> Name -> Maybe Aeson.Value
lookupVar (Env env) (Name name) =
  go env (toList name)
 where
  go o [] =
    pure o
  go (Aeson.Object o) (x : xs) = do
    o' <- HashMap.lookup x o
    go o' xs
  go _ _ =
    Nothing
