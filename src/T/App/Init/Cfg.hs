{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
module T.App.Init.Cfg
  ( Cfg(..)
  , InitTmpl(..)
  ) where

import T qualified


data Cfg = Cfg
  { tmpl        :: InitTmpl
  , env         :: T.Env
  , rootDir     :: FilePath
  , tmplDir     :: FilePath
  }

data InitTmpl
  = Name String
  | Path FilePath
    deriving (Show, Eq)
