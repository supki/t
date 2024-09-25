{-# LANGUAGE NoImplicitPrelude #-}
module T.Prelude
  ( Alternative(..)
  , Applicative(..)
  , Eq(..)
  , Fractional
  , Eq1(..)
  , Generic1
  , Hashable
  , Integral
  , IsString(..)
  , Monad(..)
  , MonadFail(..)
  , Monoid(..)
  , Num
  , Ord(..)
  , Semigroup(..)
  , Show(..)

  , Bool(..)
  , ByteString
  , Char
  , Double
  , Either(..)
  , FilePath
  , HashMap
  , Int
  , Int64
  , IO
  , Maybe(..)
  , NonEmpty(..)
  , String
  , Text
  , Vector

  , (.)
  , (<=<)
  , ($)
  , (<$)
  , (&&)
  , (+)
  , (-)
  , (*)
  , (/)
  , asum
  , bool
  , concatMap
  , const
  , div
  , either
  , eq1
  , error
  , filter
  , first
  , flip
  , foldl'
  , foldr
  , foldM_
  , foldr1
  , for_
  , fromIntegral
  , map
  , mapMaybe
  , maybe
  , not
  , notElem
  , otherwise
  , reverse
  , second
  , toList
  , traverse
  , traverse_
  , uncurry
  , unless
  , when
  , zipWith
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad ((<=<), foldM_, when, unless)
import Data.Bool (bool)
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.Foldable
  ( asum
  , for_
  , toList
  , traverse_
  )
import Data.Functor.Classes (Eq1(..), eq1)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.String (IsString(..))
import Data.Vector (Vector)
import GHC.Generics (Generic1)
import Prelude
  ( Applicative(..)
  , Eq(..)
  , Fractional
  , Functor(..)
  , Integral
  , Monad(..)
  , MonadFail(..)
  , Monoid(..)
  , Num
  , Ord(..)
  , Semigroup(..)
  , Show(..)
  , Bool(..)
  , Char
  , Double
  , Either(..)
  , FilePath
  , Int
  , IO
  , Maybe(..)
  , String
  , (.)
  , ($)
  , (<$)
  , (&&)
  , (+)
  , (-)
  , (*)
  , (/)
  , concatMap
  , const
  , div
  , either
  , error
  , filter
  , flip
  , foldr
  , foldr1
  , fromIntegral
  , maybe
  , not
  , notElem
  , otherwise
  , reverse
  , show
  , traverse
  , uncurry
  , zipWith
  )

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
