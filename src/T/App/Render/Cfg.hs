module T.App.Render.Cfg
  ( Cfg(..)
  , RenderTmpl(..)
  ) where

import T qualified
import T.Prelude


data Cfg = Cfg
  { tmpls :: [RenderTmpl]
  , env   :: T.Scope
  }

data RenderTmpl
  = String Text
  | Path FilePath
    deriving (Show, Eq)
