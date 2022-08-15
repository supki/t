module T.Error
  ( Error(..)
  ) where

import Data.Text (Text)

import T.Exp (Exp, (:+)(..), Name, Ann)


data Error
  = ShadowedBy (Ann :+ Name) (Ann :+ Name)
  | NotInScope (Ann :+ Name)
  | NotAFunction Exp
  | GenericError Text
    deriving (Show, Eq)
