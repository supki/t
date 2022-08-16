module T.Error
  ( Error(..)
  ) where

import Data.Text (Text)

import T.Exp (Exp, (:+)(..), Name, Ann)


data Error
  = ShadowedBy (Ann :+ Name) (Ann :+ Name)
  | NotInScope (Ann :+ Name)
    -- Ideally, we want a Value here instead of Text,
    -- but there's neither Show Value nor Eq Value,
    -- and that makes working with Error annoying too.
  | NotIterable Exp Text
  | NotRenderable Exp Text
  | NotAFunction Exp Text
  | GenericError Text
    deriving (Show, Eq)
