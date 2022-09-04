module T.Error
  ( Error(..)
  , Warning(..)
  ) where

import Data.Text (Text)

import T.Exp (Exp, (:+)(..), Name, Ann)


data Error
  = NotInScope (Ann :+ Name)
    -- Ideally, we want a Value here instead of Text,
    -- but there's neither Show Value nor Eq Value,
    -- and that makes working with Error annoying too.
  | NotIterable Exp Text
  | NotRenderable Exp Text
  | NotAFunction (Ann :+ Name) Text
  | GenericError Text
    deriving (Show, Eq)

data Warning
  = ShadowedBy ((Ann, Ann) :+ Name)
    deriving (Show, Eq)
