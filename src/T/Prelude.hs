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
  , MonadIO(..)
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
  , T.Prelude.error
  , filter
  , first
  , flip
  , foldl'
  , foldr
  , foldM_
  , foldr1
  , for_
  , fromIntegral
  , impossible
  , map
  , mapMaybe
  , maybe
  , not
  , notElem
  , otherwise
  , reverse
  , second
  , seq
  , toList
  , traverse
  , traverse_
  , T.Prelude.traceShow
  , uncurry
  , unless
  , when
  , zipWith
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad ((<=<), foldM_, when, unless)
import Control.Monad.IO.Class (MonadIO(..))
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
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.String (IsString(..))
import Data.Vector (Vector)
import Debug.Trace (traceShow)
import GHC.Generics (Generic1)
import GHC.Stack (HasCallStack)
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
  , seq
  , show
  , traverse
  , uncurry
  , zipWith
  )

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

impossible :: HasCallStack => a
impossible =
  Prelude.error "impossible"

error :: String -> a
error = Prelude.error
{-# DEPRECATED error "This function is only intended for debugging and should not normally be used" #-}

traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow
{-# DEPRECATED traceShow "This function is only intended for debugging and should not normally be used" #-}
