module T.Error
  ( Error(..)
  ) where

import T.Exp ((:<)(..), Name, Ann)


data Error
  = ShadowedBy (Ann :< Name) (Ann :< Name)
  | NotInScope (Ann :< Name)
  | GenericError String
    deriving (Show, Eq)
