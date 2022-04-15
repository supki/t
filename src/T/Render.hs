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

import           T.Exp (Tmpl(..), Exp(..), Name(..))


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
renderExp env = \case
  Var name ->
    case lookupVar env name of
      Nothing ->
        Left ("not in scope: " <> show name)
      Just (Aeson.String str) ->
        pure str
      Just (Aeson.Number n) ->
        pure (fromString (show n))
      Just (Aeson.Bool b) ->
        pure (bool "false" "true" b)
      Just o ->
        Left ("not renderable: " <> show o)

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
