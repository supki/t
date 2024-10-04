{-# LANGUAGE TypeFamilies #-}
module T.Embed
  ( Embed(..)
  , embed0
  , Eject(..)
  ) where

import Data.Vector qualified as Vector
import Text.Regex.PCRE.Light qualified as Pcre

import T.Error (Error(..))
import T.Exp ((:+)(..), Ann, varE)
import T.Exp.Ann (emptyAnn)
import T.Name (Name)
import T.Prelude
import T.SExp (sexp)
import T.Type qualified as Type
import T.Value (Value(..), typeOf)


class Embed t where
  -- Neither order of the arguments is ideal, unfortunately,
  -- as roughly half of applications wants the name first and
  -- another half wants it last.
  --
  -- I chose Name going first because this order mirrors the Eject class;
  -- and the idea of Name being some sort of the context in which embedding is
  -- happening makes more sense than just deciding it being the last argument
  -- because that suites stdlib definitions list better.
  embed :: Ann :+ Name -> t -> Value

instance Embed Value where
  embed _name x = x

instance Embed Bool where
  embed _name = Bool

instance Embed Int where
  embed _name =
    Int . fromIntegral

instance Embed Int64 where
  embed _name = Int

instance Embed Double where
  embed _name = Double

instance Embed Text where
  embed _name = String

instance Embed a => Embed (Maybe a) where
  embed name =
    maybe Null (embed name)

instance Embed a => Embed [a] where
  embed name =
    Array . Vector.fromList . map (embed name)

instance (Eject a, Embed b) => Embed (a -> b) where
  embed (_ :+ name) f =
    Lam (\(ann :+ x) -> map (embed (ann :+ name) . f) (eject (ann :+ name) x))

-- Some embeddings do not have a useful annotation to attach to, such as
-- stdlib definitions. This is a helper for them.
embed0 :: Embed t => Name -> t -> Value
embed0 name t =
  embed (emptyAnn :+ name) t

class Eject t where
  eject :: Ann :+ Name -> Value -> Either Error t

instance Eject Value where
  eject _name = pure

instance Eject Bool where
  eject name = \case
    Bool b ->
      pure b
    value ->
      Left (TypeError (varE name) Type.Bool (typeOf value) (sexp value))

instance Eject Int where
  eject name = \case
    Int n ->
      pure (fromIntegral n)
    value ->
      Left (TypeError (varE name) Type.Int (typeOf value) (sexp value))

instance Eject Int64 where
  eject name = \case
    Int n ->
      pure n
    value ->
      Left (TypeError (varE name) Type.Int (typeOf value) (sexp value))

instance Eject Double where
  eject name = \case
    Double n ->
      pure n
    value ->
      Left (TypeError (varE name) Type.Double (typeOf value) (sexp value))

instance Eject Text where
  eject name = \case
    String str ->
      pure str
    value ->
      Left (TypeError (varE name) Type.String (typeOf value) (sexp value))

instance Eject Pcre.Regex where
  eject name = \case
    Regexp regexp ->
      pure regexp
    value ->
      Left (TypeError (varE name) Type.Regexp (typeOf value) (sexp value))

instance (k ~ Text, v ~ Value) => Eject (HashMap k v) where
  eject name = \case
    Record o ->
      pure o
    value ->
      Left (TypeError (varE name) Type.Record (typeOf value) (sexp value))

instance Eject a => Eject [a] where
  eject name = \case
    Array xs ->
      map toList (traverse (eject name) xs)
    value ->
      Left (TypeError (varE name) Type.Array (typeOf value) (sexp value))
