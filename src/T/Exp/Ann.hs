{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T.Exp.Ann
  ( (:<)(..)
  , Span(..)
  , anned
  , noann
  , unann
  ) where

import Data.String (IsString(..))
import Prelude hiding (span)
import Text.Trifecta (DeltaParsing, Span(..), Spanned(..), spanned)


-- | Just a pair. The annotation part is ignored
-- when checking for equality.
data ann :< t = ann :< t
    deriving (Show)

-- | This instance ignores the annotation part.
instance Eq t => Eq (ann :< t) where
  _ann0 :< t0 == _ann1 :< t1 =
    t0 == t1

instance (IsString t, ann ~ Span) => IsString (ann :< t) where
  fromString str =
    emptySpan :< fromString str

anned :: DeltaParsing m => m a -> m (Span :< a)
anned p = do
  a :~ span <- spanned p
  pure (span :< a)

noann :: a -> Span :< a
noann a =
  emptySpan :< a

unann :: ann :< t -> t
unann (_ann :< t) = t

emptySpan :: Span
emptySpan =
  Span mempty mempty mempty
