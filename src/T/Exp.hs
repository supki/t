{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module T.Exp
  ( Exp
  , Cofree(..)
  , ExpF(..)
  , Literal(..)
  , Name(..)
  , (:+)(..)
  , Ann
  , litE
  , litE_
  , varE
  , varE_
  , ifE
  , ifE_
  , appE
  , appE_
  , falseL
  , trueL
  ) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Functor.Classes (Eq1(..), eq1)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic1)
import Prelude hiding (exp)
import Prettyprinter (Pretty(..))
import Text.Regex.PCRE.Light qualified as Pcre

import T.Exp.Ann (Ann, (:+)(..), emptyAnn)


data Cofree f a = a :< f (Cofree f a)

deriving instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a)

instance Eq1 f => Eq (Cofree f a) where
  (_ :< f) == (_ :< g) =
    eq1 f g

instance Aeson.ToJSON1 f => Aeson.ToJSON (Cofree f a) where
  toJSON (_ :< f) =
    Aeson.toJSON1 f

type Exp = Cofree ExpF Ann

data ExpF a
  = Lit Literal
  | Var (Ann :+ Name)
  | If a a a
  | App (Ann :+ Name) (NonEmpty a)
    deriving (Show, Eq, Generic1)

instance Eq1 ExpF where
  liftEq _ (Lit l0) (Lit l1) =
    l0 == l1
  liftEq _ (Var v0) (Var v1) =
    v0 == v1
  liftEq (==?) (If p0 t0 f0) (If p1 t1 f1) =
    (p0 ==? p1) && (t0 ==? t1) && (f0 ==? f1)
  liftEq (==?) (App n0 as0) (App n1 as1) =
    (n0 == n1) && and (NonEmpty.zipWith (==?) as0 as1)
  liftEq _ _ _ =
    False

instance Aeson.ToJSON1 ExpF where
  liftToJSON f _fxs =
    Aeson.object . \case
      Lit value ->
        [ "variant" .= ("lit" :: Text)
        , "value" .= value
        ]
      Var name ->
        [ "variant" .= ("var" :: Text)
        , "name" .= name
        ]
      If p expt expf ->
        [ "variant" .= ("if" :: Text)
        , "predicate" .= f p
        , "when-true" .= f expt
        , "when-false" .= f expf
        ]
      App name args ->
        [ "variant" .= ("app" :: Text)
        , "name" .= name
        , "args" .= fmap f args
        ]

instance Aeson.ToJSON a => Aeson.ToJSON (ExpF a) where
  toJSON = Aeson.toJSON1

litE :: Ann -> Literal -> Exp
litE ann lit =
  ann :< Lit lit

litE_ :: Literal -> Exp
litE_ =
  litE emptyAnn

varE :: Ann -> Ann :+ Name -> Exp
varE ann name =
  ann :< Var name

varE_ :: Ann :+ Name -> Exp
varE_ =
  varE emptyAnn

ifE :: Ann -> Exp -> Exp -> Exp -> Exp
ifE ann p t f =
  ann :< If p t f

ifE_ :: Exp -> Exp -> Exp -> Exp
ifE_ =
  ifE emptyAnn

appE :: Ann -> Ann :+ Name -> NonEmpty Exp -> Exp
appE ann name args =
  ann :< App name args

appE_ :: Ann :+ Name -> NonEmpty Exp -> Exp
appE_ =
  appE emptyAnn

data Literal
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Regexp Pcre.Regex
  | Array (Vector Exp)
  | Object (HashMap Text Exp)
    deriving (Show, Eq)

instance Aeson.ToJSON Literal where
  toJSON =
    Aeson.object . \case
      Null ->
        [ "variant" .= ("null" :: Text)
        ]
      Bool value ->
        [ "variant" .= ("bool" :: Text)
        , "value" .= value
        ]
      Number value ->
        [ "variant" .= ("number" :: Text)
        , "value" .= value
        ]
      String value ->
        [ "variant" .= ("string" :: Text)
        , "value" .= value
        ]
      Regexp value ->
        [ "variant" .= ("regexp" :: Text)
        , "value" .= show value
        ]
      Array value ->
        [ "variant" .= ("array" :: Text)
        , "value" .= value
        ]
      Object value ->
        [ "variant" .= ("object" :: Text)
        , "value" .= value
        ]

falseL :: Literal
falseL =
  Bool False

trueL :: Literal
trueL =
  Bool True
  
newtype Name = Name { unName :: Text }
    deriving (Show, Eq, Ord, IsString, Hashable)

instance Pretty Name where
  pretty Name {unName} =
    pretty unName

instance Aeson.ToJSON Name where
  toJSON =
    Aeson.toJSON . unName
