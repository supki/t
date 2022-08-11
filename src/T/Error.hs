module T.Error
  ( Error(..)
  ) where

import Data.Text (Text)

import T.Exp ((:<)(..), Name, Ann)


data Error
  = ShadowedBy (Ann :< Name) (Ann :< Name)
  | NotInScope (Ann :< Name)
  | GenericError Text
    deriving (Show, Eq)
