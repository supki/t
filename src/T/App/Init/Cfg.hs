{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
module T.App.Init.Cfg
  ( Cfg(..)
  ) where

import T qualified


data Cfg = Cfg
  { tmpl        :: FilePath
  , env         :: T.Env
  , rootDir     :: FilePath
  , tmplDir     :: FilePath
  , skipTestRun :: Bool
  }
