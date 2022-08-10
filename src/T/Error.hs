{-# LANGUAGE TypeOperators #-}
module T.Error
  ( Error(..)
  ) where

import Text.Trifecta (Span)

import T.Exp ((:<)(..), Name)


data Error
  = ShadowedBy (Span :< Name) (Span :< Name)
  | GenericError String
    deriving (Show, Eq)
