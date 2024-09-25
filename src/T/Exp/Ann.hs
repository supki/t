{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module T.Exp.Ann
  ( (:+)(..)
  , Ann
  , anning
  , anned
  , noann
  , unann
  , emptyAnn
  ) where

import Text.Trifecta (DeltaParsing, Span(..), Spanned(..), spanning, spanned)

import T.Prelude


type Ann = Span

-- | Just a pair. The annotation part is ignored
-- when checking for equality.
data ann :+ t = ann :+ t
    deriving (Show)

-- | This instance ignores the annotation part.
instance Eq t => Eq (ann :+ t) where
  _ann0 :+ t0 == _ann1 :+ t1 =
    t0 == t1

-- | This instance ignores the annotation part.
instance Ord t => Ord (ann :+ t) where
  (_ann0 :+ t0) `compare` (_ann1 :+ t1) =
    t0 `compare` t1

instance IsString t => IsString (Ann :+ t) where
  fromString str =
    emptyAnn :+ fromString str

instance IsString t => IsString ((Ann, Ann) :+ t) where
  fromString str =
    (emptyAnn, emptyAnn) :+ fromString str

anning :: DeltaParsing m => m a -> m Ann
anning =
  spanning

anned :: DeltaParsing m => m a -> m (Ann :+ a)
anned p = do
  a :~ span <- spanned p
  pure (span :+ a)

noann :: a -> Ann :+ a
noann a =
  emptyAnn :+ a

unann :: ann :+ t -> t
unann (_ann :+ t) = t

emptyAnn :: Ann
emptyAnn =
  Span mempty mempty mempty
